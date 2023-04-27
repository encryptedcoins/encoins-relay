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

import           CSL                            (TransactionUnspentOutputs)
import           CSL.Class                      (ToCSL (..))
import           Cardano.Server.Client.Handle   (ClientHandle (..), HasServantClientEnv, autoWith, manualWith)
import           Cardano.Server.Client.Internal (ClientEndpoint (..), Interval, ServerEndpoint (NewTxE, ServerTxE))
import           Cardano.Server.Internal        (InputOf, ServerM, getNetworkId)
import           Cardano.Server.Utils.Logger    (logMsg)
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Applicative            (liftA2)
import           Control.Monad                  (join)
import           Control.Monad.Extra            (forever, replicateM, zipWithM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Data                      (Proxy (Proxy))
import           Data.Default                   (def)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           ENCOINS.BaseTypes              (MintingPolarity (Burn, Mint))
import           ENCOINS.Bulletproofs           (Secret (..), bulletproof, fromSecret, parseBulletproofParams)
import           ENCOINS.Core.OnChain           (TxParams)
import           ENCOINS.Core.V1.OffChain       (EncoinsMode (..))
import           ENCOINS.Crypto.Field           (fromFieldElement, toFieldElement)
import           Encoins.Relay.Client.Opts      (EncoinsRequestTerm (..), readTerms)
import           Encoins.Relay.Client.Secrets   (clientSecretToSecret, confirmTokens, genTerms, mkSecretFile, readSecretFile)
import           Encoins.Relay.Server.Server    (EncoinsApi, bulletproofSetup, getLedgerAddress)
import           Ledger.Ada                     (Ada (getLovelace))
import           Ledger.Value                   (TokenName (..))
import           PlutusAppsExtra.IO.ChainIndex  (getUtxosAt)
import           PlutusAppsExtra.IO.Wallet      (getWalletAddr, getWalletUtxos)
import           PlutusTx.Builtins              (sha2_256)
import           PlutusTx.Extra.ByteString      (ToBuiltinByteString (..))
import           Servant.Client                 (runClientM)
import qualified Servant.Client                 as Servant
import           System.Random                  (randomIO, randomRIO)

mkClientHandle :: EncoinsMode -> ClientHandle EncoinsApi
mkClientHandle mode = let ?mode = mode in def
    { autoNewTx      = \i -> forever $ genTerms >>= txClient @'NewTxE >> waitI i
    , autoServerTx   = \i -> forever $ join (genTerms >>= txClient @'ServerTxE) >> waitI i
    , autoStatus     = autoWith (pure ())
    , manualNewTx    = \txt -> Proxy <$ manualTxClient @'NewTxE txt
    , manualServerTx = \txt -> Proxy <$ join (manualTxClient @'ServerTxE txt)
    , manualStatus   = manualWith (const $ pure ())
    }
    where waitI i = randomRIO (1, i * 2) >>= waitTime

type HasEncoinsMode = ?mode :: EncoinsMode

type TxClientCosntraints (e :: ServerEndpoint) =
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (InputOf EncoinsApi, TransactionUnspentOutputs)
    , HasServantClientEnv
    , HasEncoinsMode
    )

autoTxClient :: forall e. TxClientCosntraints e => Interval -> ServerM EncoinsApi (Proxy e)
autoTxClient i = forever $ do
    genTerms >>= txClient @e
    waitTime =<< randomRIO (1, i * 2)

manualTxClient :: forall e. TxClientCosntraints e => Text -> ServerM EncoinsApi (ServerM EncoinsApi ())
manualTxClient txt = txClient @e (fromMaybe (error "Unparsable input.") $ readTerms txt)

txClient :: forall e. TxClientCosntraints e => [EncoinsRequestTerm] -> ServerM EncoinsApi (ServerM EncoinsApi ())
txClient terms = do
    secrets <- termsToSecrets terms
    res <- sendTxClientRequest @e secrets
    let processFiles = mapM_ (uncurry mkSecretFile) secrets >> confirmTokens
    pure $ either (const $ pure ()) (const processFiles) res

sendTxClientRequest :: forall e . TxClientCosntraints e
    => [(Secret, MintingPolarity)] -> ServerM EncoinsApi (Either Servant.ClientError (EndpointRes e EncoinsApi))
sendTxClientRequest secrets = do
    reqBody@(((_,(v, inputs),_,_),_),_) <- secretsToReqBody secrets
    logMsg $ "Sending request with:\n"
            <> foldl prettyInput "" (zip (map fst secrets) inputs)
            <> "\n= "
            <> T.pack (show v)
    res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @EncoinsApi reqBody)
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

secretsToReqBody :: HasEncoinsMode => [(Secret, MintingPolarity)] -> ServerM EncoinsApi (InputOf EncoinsApi, TransactionUnspentOutputs)
secretsToReqBody (unzip -> (secrets, ps)) = do
    randomness <- randomIO
    networkId  <- getNetworkId
    walletAddr <- getWalletAddr
    ledgerAddr <- getLedgerAddress
    outputs    <- fromMaybe [] . toCSL . (,networkId) . Map.toList <$> case ?mode of
        WalletMode -> getWalletUtxos
        LedgerMode -> liftA2 (<>) getWalletUtxos (getLedgerAddress >>= getUtxosAt)
    let par = (ledgerAddr, walletAddr) :: TxParams
        bp   = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
        (v, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        signature  = ""
    pure (((par, (v, inputs), proof, signature), ?mode), outputs)

termsToSecrets :: [EncoinsRequestTerm] -> ServerM EncoinsApi [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> (, Burn) . clientSecretToSecret . fromMaybe (error fp) <$> readSecretFile fp