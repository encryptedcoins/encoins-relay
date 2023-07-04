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

import           CSL                            (TransactionInputs)
import qualified CSL
import           CSL.Class                      (ToCSL (..))
import           Cardano.Server.Client.Handle   (ClientHandle (..), HasServantClientEnv, autoWithRandom, manualWithRead)
import           Cardano.Server.Client.Internal (ClientEndpoint (..), ServerEndpoint (NewTxE, ServerTxE))
import           Cardano.Server.Internal        (InputOf, ServerM)
import           Cardano.Server.Utils.Logger    (logMsg, (.<))
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Applicative            (liftA2)
import           Control.Monad                  (join)
import           Control.Monad.Extra            (forever, replicateM, zipWithM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Data                      (Proxy (Proxy))
import           Data.Default                   (def)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           ENCOINS.BaseTypes              (MintingPolarity (Burn, Mint))
import           ENCOINS.Bulletproofs           (Secret (..), bulletproof, fromSecret, parseBulletproofParams, polarityToInteger)
import           ENCOINS.Core.OffChain          (EncoinsMode (..), protocolFee)
import           ENCOINS.Core.OnChain           (EncoinsRedeemer, TxParams)
import           ENCOINS.Crypto.Field           (fromFieldElement, toFieldElement)
import           Encoins.Relay.Client.Opts      (EncoinsRequestTerm (..), readAddressValue, readRequestTerms)
import           Encoins.Relay.Client.Secrets   (HasEncoinsMode, clientSecretToSecret, confirmTokens, genTerms, mkSecretFile,
                                                 readSecretFile)
import           Encoins.Relay.Server.Internal  (getLedgerAddress)
import           Encoins.Relay.Server.Server    (EncoinsApi)
import           Encoins.Relay.Verifier.Server  (bulletproofSetup)
import           Ledger                         (Address)
import           Ledger.Ada                     (Ada (getLovelace))
import           Ledger.Value                   (TokenName (..))
import           PlutusAppsExtra.IO.ChainIndex  (getRefsAt)
import           PlutusAppsExtra.IO.Wallet      (getWalletAddr, getWalletRefs)
import           PlutusTx.Builtins              (sha2_256)
import           PlutusTx.Extra.ByteString      (ToBuiltinByteString (..))
import           Servant.Client                 (runClientM)
import qualified Servant.Client                 as Servant
import           System.Random                  (randomIO, randomRIO)

mkClientHandle :: EncoinsMode -> ClientHandle EncoinsApi
mkClientHandle mode = let ?mode = mode in def
    { autoNewTx      = \i -> forever $ genTerms >>= txClientRedeemer @'NewTxE >> waitI i
    , autoServerTx   = \i -> forever $ join (genTerms >>= txClientRedeemer @'ServerTxE) >> waitI i
    , autoStatus     = autoWithRandom
    , manualNewTx    = \txt -> Proxy <$ manualTxClient @'NewTxE txt
    , manualServerTx = \txt -> Proxy <$ join (manualTxClient @'ServerTxE txt)
    , manualStatus   = manualWithRead
    }
    where waitI i = randomRIO (1, i * 2) >>= waitTime

type TxClientCosntraints (e :: ServerEndpoint) =
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (InputOf EncoinsApi, TransactionInputs)
    , HasServantClientEnv
    )

manualTxClient :: forall e. (TxClientCosntraints e, HasEncoinsMode) => Text -> ServerM EncoinsApi (ServerM EncoinsApi ())
manualTxClient = \case
    (readRequestTerms -> Just reqTerms) -> txClientRedeemer @e reqTerms
    (readAddressValue -> Just addrVal)  -> pure <$> txClientAddressValue @e addrVal
    _ -> error "Unparsable input."

------------------------------------------------------------------------- TxClient with (Address, Value) -------------------------------------------------------------------------

txClientAddressValue :: forall e. TxClientCosntraints e => (Address, CSL.Value) -> ServerM EncoinsApi ()
txClientAddressValue (addr, val) = do
    changeAddr <- getWalletAddr
    txInputs <- fromMaybe [] . toCSL <$> getRefsAt changeAddr
    logMsg $ "Sending request with:" .< ((addr, val), txInputs)
    res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @EncoinsApi $ (Left (addr, val, changeAddr), txInputs))
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res

----------------------------------------------------------------------------- TxClient with redeemer -----------------------------------------------------------------------------

txClientRedeemer :: forall e. (TxClientCosntraints e, HasEncoinsMode) => [EncoinsRequestTerm] -> ServerM EncoinsApi (ServerM EncoinsApi ())
txClientRedeemer terms = do
    secrets <- termsToSecrets terms
    res <- sendTxClientRequest @e secrets
    let processFiles = mapM_ (uncurry mkSecretFile) secrets >> confirmTokens
    pure $ either (const confirmTokens) (const processFiles) res

sendTxClientRequest :: forall e . (TxClientCosntraints e, HasEncoinsMode)
    => [(Secret, MintingPolarity)] -> ServerM EncoinsApi (Either Servant.ClientError (EndpointRes e EncoinsApi))
sendTxClientRequest secrets = do
    (red@(_,(v, inputs),_,_),txInputs) <- secretsToReqBody secrets
    logMsg $ "Sending request with:\n"
            <> foldl prettyInput "" (zip (map fst secrets) inputs)
            <> "\n= "
            <> T.pack (show v)
    res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @EncoinsApi $ (Right (red, ?mode), txInputs))
    logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
    pure res
    where
        prettyInput acc (Secret _ v,(bbs, p)) = mconcat
            [ acc
            , "\n"
            , case p of {Mint -> "M "; Burn -> "B "}
            , T.pack $ show $ fromFieldElement v
            , " "
            , T.pack $ show $ TokenName bbs
            ]

secretsToReqBody :: HasEncoinsMode => [(Secret, MintingPolarity)] -> ServerM EncoinsApi (EncoinsRedeemer, TransactionInputs)
secretsToReqBody (unzip -> (secrets, ps)) = do
    randomness <- randomIO
    walletAddr <- getWalletAddr
    ledgerAddr <- getLedgerAddress
    txInputs    <- fromMaybe [] . toCSL <$> case ?mode of
        WalletMode -> getWalletRefs
        LedgerMode -> liftA2 (<>) getWalletRefs (getLedgerAddress >>= getRefsAt)
    liftIO $ print txInputs
    let (tokenValsAbs, tokenNames) = unzip $ map (fromSecret bulletproofSetup) secrets
        v              = sum $ zipWith (*) (map polarityToInteger ps) tokenValsAbs
        par           = (ledgerAddr, walletAddr, 2*protocolFee ?mode v) :: TxParams
        bp            = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs        = zip tokenNames ps
        (_, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        signature  = ""
    pure ((par, (v, inputs), proof, signature), txInputs)

termsToSecrets :: [EncoinsRequestTerm] -> ServerM EncoinsApi [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> (, Burn) . clientSecretToSecret . fromMaybe (fpError fp) <$> readSecretFile fp
        fpError = error . ("Secret file doesn't exists:\n" <>)