{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Encoins.Relay.Client.TxClient where

import           CSL                           (TransactionInputs)
import qualified CSL
import           CSL.Class                     (ToCSL (..))
import           Encoins.Relay.Common.Client   (HasServantClientEnv)
import           ENCOINS.BaseTypes             (MintingPolarity (Burn, Mint))
import           ENCOINS.Bulletproofs          (Secret (..), bulletproof, fromSecret, parseBulletproofParams, polarityToInteger)
import           ENCOINS.Core.OffChain         (EncoinsMode (..), protocolFee, treasuryFee)
import           ENCOINS.Core.OnChain          (EncoinsRedeemer, TxParams)
import           ENCOINS.Crypto.Field          (fromFieldElement, toFieldElement)
import           Encoins.Common.Constant       (newLine, space)
import           Encoins.Common.Log
import           Encoins.Common.Transform      (toText)
import           Encoins.Relay.Client.Secrets  (EncoinsRequestTerm (..), HasEncoinsModeAndBulletproofSetup, clientSecretToSecret,
                                                confirmTokens, mkSecretFile, readSecretFile)
import           Encoins.Relay.Server.Internal (EncoinsServerInput (..), EncoinsServerM, getLedgerAddress)
import           Ledger                        (Address)
import           Plutus.Script.Utils.Ada       (Ada (..))
import           Plutus.V2.Ledger.Api          (TokenName (..))
import           PlutusAppsExtra.IO.ChainIndex (getRefsAt)
import           PlutusAppsExtra.IO.Wallet     (getWalletAddr, getWalletRefs)
import           PlutusTx.Builtins             (sha2_256)
import           PlutusTx.Extra.ByteString     (ToBuiltinByteString (..))

import           Control.Applicative           (liftA2)
import           Control.Monad.Extra           (replicateM, zipWithM)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Data                     (Proxy (Proxy))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Servant.Client                (ClientM, client, runClientM)
import qualified Servant.Client                as Servant
import           System.Random                 (randomIO)

type TxClientCosntraints e res =
    ( HasServantClientEnv
    , Show res
    , Servant.Client ClientM e ~ ((EncoinsServerInput, TransactionInputs) -> ClientM res)
    , Servant.HasClient ClientM e
    )

------------------------------------------------------------------------- TxClient with (Address, Value) -------------------------------------------------------------------------

txClientAddressValue :: forall e res. TxClientCosntraints e res => (Address, CSL.Value) -> EncoinsServerM (Either Servant.ClientError res)
txClientAddressValue (addr, val) = do
    changeAddr <- getWalletAddr
    txInputs <- fromMaybe [] . toCSL <$> getRefsAt changeAddr
    logInfo "Sending request with:"
    logInfoS True ((addr, val), txInputs)
    res <- liftIO (flip runClientM ?servantClientEnv $ client (Proxy @e) (InputSending addr val changeAddr, txInputs))
    logInfo $ "Received response:\n" <> either toText toText res
    pure res

------------------------------------------------------------------------- TxClient with (Address, IpAddress) -------------------------------------------------------------------------

txClientDelegation :: forall e res. TxClientCosntraints e res => (Address, Text) -> EncoinsServerM (Either Servant.ClientError res)
txClientDelegation (addr, ipAddr) = do
    txInputs <- fromMaybe [] . toCSL <$> getRefsAt addr
    logInfo "Sending request with:"
    logInfoS True (addr, ipAddr)
    res <- liftIO (flip runClientM ?servantClientEnv $ client (Proxy @e) (InputDelegation addr ipAddr, txInputs))
    logInfo $ "Received response:\n" <> either toText toText res
    pure res

----------------------------------------------------------------------------- TxClient with redeemer -----------------------------------------------------------------------------

txClientRedeemer :: forall e res. (TxClientCosntraints e res, HasEncoinsModeAndBulletproofSetup) => [EncoinsRequestTerm] -> EncoinsServerM (EncoinsServerM (), Either Servant.ClientError res)
txClientRedeemer terms = do
    secrets <- termsToSecrets terms
    res <- sendTxClientRequest @e @res secrets
    let processFiles = mapM_ (uncurry mkSecretFile) secrets >> confirmTokens
    pure (either (const confirmTokens) (const processFiles) res, res)

sendTxClientRequest :: forall e res. (TxClientCosntraints e res, HasEncoinsModeAndBulletproofSetup)
    => [(Secret, MintingPolarity)] -> EncoinsServerM (Either Servant.ClientError res)
sendTxClientRequest secrets = do
    (red@(_,(v, inputs),_,_),txInputs) <- secretsToReqBody secrets
    logInfo $ "Sending request with:\n"
            <> foldl prettyInput "" (zip (map fst secrets) inputs)
            <> "\n= "
            <> toText v
    res <- liftIO (flip runClientM ?servantClientEnv $ client (Proxy @e) $ (InputRedeemer red ?mode, txInputs))
    logInfo $ "Received response:\n" <> either toText toText res
    pure res
    where
        prettyInput acc (Secret _ v,(bbs, p)) = mconcat
            [ acc
            , newLine
            , case p of Mint -> "M" <> space; Burn -> "B" <> space
            , toText $ fromFieldElement v
            , space
            , toText $ TokenName bbs
            ]

secretsToReqBody :: HasEncoinsModeAndBulletproofSetup => [(Secret, MintingPolarity)] -> EncoinsServerM (EncoinsRedeemer, TransactionInputs)
secretsToReqBody (unzip -> (secrets, ps)) = do
    randomness <- randomIO
    walletAddr <- getWalletAddr
    ledgerAddr <- getLedgerAddress
    txInputs    <- fromMaybe [] . toCSL <$> case ?mode of
        WalletMode -> getWalletRefs
        LedgerMode -> liftA2 (<>) getWalletRefs (getLedgerAddress >>= getRefsAt)
    let (tokenValsAbs, tokenNames) = unzip $ map (fromSecret ?bulletproofSetup) secrets
        v              = sum $ zipWith (*) (map polarityToInteger ps) tokenValsAbs
        par           = (ledgerAddr, walletAddr, treasuryFee ?mode v + protocolFee ?mode v) :: TxParams
        bp            = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs        = zip tokenNames ps
        (_, _, proof) = bulletproof ?bulletproofSetup bp secrets ps randomness
        signature  = ""
    pure ((par, (v, inputs), proof, signature), txInputs)

termsToSecrets :: [EncoinsRequestTerm] -> EncoinsServerM [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> (, Burn) . clientSecretToSecret . fromMaybe (fpError fp) <$> readSecretFile fp
        fpError = error . ("Secret file doesn't exists:\n" <>)
