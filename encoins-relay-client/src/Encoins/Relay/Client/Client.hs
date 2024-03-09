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

module Encoins.Relay.Client.Client where

import           CSL                                        (TransactionInputs)
import qualified CSL
import           CSL.Class                                  (ToCSL (..))
import           Cardano.Server.Internal                    (AppT)
import           Cardano.Server.Utils.Logger                (logMsg, (.<))
import           Control.Applicative                        (liftA2)
import           Control.Monad.Extra                        (replicateM, zipWithM)
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Data.Data                                  (Proxy (Proxy))
import           Data.Maybe                                 (fromMaybe)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           ENCOINS.BaseTypes                          (MintingPolarity (Burn, Mint))
import           ENCOINS.Bulletproofs                       (Secret (..), bulletproof, fromSecret, parseBulletproofParams,
                                                             polarityToInteger)
import           ENCOINS.Core.OffChain                      (EncoinsMode (..), protocolFee, treasuryFee)
import           ENCOINS.Core.OnChain                       (EncoinsRedeemer, TxParams)
import           ENCOINS.Crypto.Field                       (fromFieldElement, toFieldElement)
import           Encoins.Relay.Client.Secrets               (EncoinsRequestTerm (..), HasEncoinsModeAndBulletproofSetup,
                                                             clientSecretToSecret, confirmTokens, mkSecretFile, readSecretFile)
import           Encoins.Relay.Server.Internal              (EncoinsRelayEnv (..), getLedgerAddress)
import           Encoins.Relay.Server.Server                (EncoinsApi)
import           Ledger                                     (Address)
import           Plutus.Script.Utils.Ada                    (Ada (..))
import           Plutus.V2.Ledger.Api                       (TokenName (..))

import           Cardano.Server.Client.Client               (HasServantClientEnv)
import           Cardano.Server.Error.Servant               (EndpointEnvelope)
import           Control.Monad.Catch                        (MonadCatch)
import           Encoins.Relay.Server.Endpoints.Tx.Intenral (InputOfEncoinsApi (..))
import           PlutusAppsExtra.IO.ChainIndex              (getRefsAt)
import           PlutusAppsExtra.IO.Wallet                  (getWalletAddr, getWalletRefs)
import           PlutusTx.Builtins                          (sha2_256)
import           PlutusTx.Extra.ByteString                  (ToBuiltinByteString (..))
import           Servant.Client                             (ClientError, ClientM, HasClient (..), client, runClientM)
import qualified Servant.Client                             as Servant
import           System.Random                              (randomIO)

type TxClientCosntraints e =
    ( Client ClientM e ~ ((InputOfEncoinsApi, CSL.TransactionInputs) -> ClientM (EndpointEnvelope e))
    , HasEncoinsModeAndBulletproofSetup
    , HasServantClientEnv
    , HasClient ClientM e
    , Show (EndpointEnvelope e)
    )
------------------------------------------------------------------------- TxClient with (Address, Value) -------------------------------------------------------------------------

txClientAddressValue :: forall e m. (TxClientCosntraints e, MonadIO m, MonadCatch m)
    => (Address, CSL.Value) -> AppT EncoinsApi m (Either Servant.ClientError (EndpointEnvelope e))
txClientAddressValue (addr, val) = do
    changeAddr <- getWalletAddr
    txInputs <- fromMaybe [] . toCSL <$> getRefsAt changeAddr
    logMsg $ "Sending request with:" .< ((addr, val), txInputs)
    res <- liftIO (flip runClientM ?servantClientEnv $ client @e (Proxy @e) $ (InputSending addr val changeAddr, txInputs))
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
    pure res

------------------------------------------------------------------------- TxClient with (Address, IpAddress) -------------------------------------------------------------------------

txClientDelegation :: forall e m. (TxClientCosntraints e, MonadIO m, MonadCatch m)
    => (Address, Text) -> AppT EncoinsApi m (Either Servant.ClientError (EndpointEnvelope e))
txClientDelegation (addr, ipAddr) = do
    txInputs <- fromMaybe [] . toCSL <$> getRefsAt addr
    logMsg $ "Sending request with:" .< (addr, ipAddr)
    res <- liftIO (flip runClientM ?servantClientEnv $ client @e (Proxy @e) (InputDelegation addr ipAddr, txInputs))
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
    pure res

----------------------------------------------------------------------------- TxClient with redeemer -----------------------------------------------------------------------------

txClientRedeemer :: forall e m. (TxClientCosntraints e, MonadIO m, MonadCatch m)
    => [EncoinsRequestTerm] -> AppT EncoinsApi m (AppT EncoinsApi m (), Either ClientError (EndpointEnvelope e))
txClientRedeemer terms = do
    secrets <- termsToSecrets @m terms
    res <- sendTxClientRequest @e secrets
    let processFiles = mapM_ (uncurry mkSecretFile) secrets >> confirmTokens
    pure $ (, res) $ either (const confirmTokens) (const processFiles) res

sendTxClientRequest :: forall e m. (TxClientCosntraints e, MonadIO m, MonadCatch m)
    => [(Secret, MintingPolarity)] -> AppT EncoinsApi m (Either Servant.ClientError (EndpointEnvelope e))
sendTxClientRequest secrets = do
    (red@(_,(v, inputs),_,_),txInputs) <- secretsToReqBody secrets
    logMsg $ "Sending request with:\n"
            <> foldl prettyInput "" (zip (map fst secrets) inputs)
            <> "\n= "
            <> T.pack (show v)
    res <- liftIO (flip runClientM ?servantClientEnv $ client @e (Proxy @e) (InputRedeemer red ?mode, txInputs))
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
    pure res
    where
        prettyInput acc (Secret _ v,(bbs, p)) = mconcat
            [ acc
            , "\n"
            , case p of Mint -> "M "; Burn -> "B "
            , T.pack $ show $ fromFieldElement v
            , " "
            , T.pack $ show $ TokenName bbs
            ]

secretsToReqBody :: (HasEncoinsModeAndBulletproofSetup, MonadIO m, MonadCatch m)
    => [(Secret, MintingPolarity)] -> AppT EncoinsApi m (EncoinsRedeemer, TransactionInputs)
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

termsToSecrets :: MonadIO m => [EncoinsRequestTerm] -> AppT EncoinsApi m [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> (, Burn) . clientSecretToSecret . fromMaybe (fpError fp) <$> readSecretFile fp
        fpError = error . ("Secret file doesn't exists:\n" <>)