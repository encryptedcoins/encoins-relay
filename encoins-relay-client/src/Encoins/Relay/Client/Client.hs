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

module  Encoins.Relay.Client.Client where

import           CSL                            (TransactionUnspentOutputs)
import           CSL.Class                      (ToCSL (..))
import           Cardano.Server.Client.Handle   (ClientHandle (..), HasServantClientEnv, autoWith, manualWith)
import           Cardano.Server.Client.Internal (ClientEndpoint, EndpointArg, EndpointRes, Interval, ServerEndpoint,
                                                 endpointClient)
import           Cardano.Server.Internal        (InputOf, ServerM, getAuxillaryEnv, getNetworkId)
import           Cardano.Server.Tx              (checkForCleanUtxos)
import           Cardano.Server.Utils.Logger    (logMsg)
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Monad.Extra            (filterM, forever, ifM, replicateM, zipWithM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.State            (MonadState (..), evalStateT, modify, when)
import           Data.Data                      (Proxy (Proxy))
import           Data.Default                   (def)
import           Data.Either                    (isRight)
import           Data.Functor                   ((<&>))
import           Data.List                      (delete)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           ENCOINS.BaseTypes              (MintingPolarity (Burn, Mint))
import           ENCOINS.Bulletproofs           (Secret (..), bulletproof, fromSecret, parseBulletproofParams)
import           ENCOINS.Core.OnChain           (TxParams, beaconCurrencySymbol, encoinsSymbol)
import           ENCOINS.Core.V1.OffChain       (EncoinsMode (..))
import           ENCOINS.Crypto.Field           (Field, FiniteField, fromFieldElement, toFieldElement)
import           Encoins.Relay.Client.Opts      (EncoinsRequestTerm (..), readTerms)
import           Encoins.Relay.Server.Server    (EncoinsApi, bulletproofSetup, getTrackedAddresses, verifierPKH)
import           Ledger.Ada                     (Ada (getLovelace), lovelaceOf)
import           Plutus.V1.Ledger.Api           (TokenName (TokenName), Value (getValue), fromBuiltin)
import           PlutusAppsExtra.IO.Wallet      (getWalletAddr, getWalletUtxos, getWalletValue)
import qualified PlutusTx.AssocMap              as PAM
import           PlutusTx.Builtins              (sha2_256)
import           PlutusTx.Extra.ByteString      (ToBuiltinByteString (..))
import           Servant.Client                 (runClientM)
import qualified Servant.Client                 as Servant
import           System.Directory               (listDirectory, removeFile)
import           System.Random                  (randomIO, randomRIO)
import           Text.Hex                       (encodeHex)
import           Text.Read                      (readMaybe)

clientHandle :: ClientHandle EncoinsApi
clientHandle = def
    { autoNewTx      = autoTxClient
    , autoServerTx   = autoTxClient
    , autoStatus     = autoWith (pure ())
    , manualNewTx    = manualTxClient
    , manualServerTx = manualTxClient
    , manualStatus   = manualWith (const $ pure ())
    }

type TxClientCosntraints (e :: ServerEndpoint) =
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (InputOf EncoinsApi, TransactionUnspentOutputs)
    , HasServantClientEnv
    )

autoTxClient :: forall e. TxClientCosntraints e => Interval -> ServerM EncoinsApi (Proxy e)
autoTxClient i = forever $ do
    genTerms >>= txClient @e
    waitTime =<< randomRIO (1, i * 2)

manualTxClient :: forall e. TxClientCosntraints e => Text -> ServerM EncoinsApi (Proxy e)
manualTxClient txt = Proxy <$ txClient @e (fromMaybe (error "Unparsable input.") $ readTerms txt)

txClient :: forall e. TxClientCosntraints e => [EncoinsRequestTerm] -> ServerM EncoinsApi (Proxy (e :: ServerEndpoint))
txClient terms = Proxy <$ do
    checkForCleanUtxos
    secrets <- termsToSecrets terms
    res     <- sendTxClientRequest @e secrets
    when (isRight res) $ mapM_ processFile secrets

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
            , showPolarity p
            , T.pack (feToString v)
            , " "
            , encodeHex (fromBuiltin bbs)
            ]
        showPolarity = \case {Mint -> "M "; Burn -> "B "}

genTerms :: ServerM EncoinsApi [EncoinsRequestTerm]
genTerms = do
    minted  <- getWalletEncoinsTokens
    secrets <- liftIO $ listDirectory "secrets" >>= filterM (alreadyMinted minted)
    l       <- randomRIO (1, 5)
    (`evalStateT` secrets) $ replicateM l genTerm
    where
        genTerm = get >>= \secrets -> ifM ((null secrets ||) <$> randomIO) randomMintTerm $ do
            secret <- (secrets!!) <$> (randomIO <&> (`mod` length secrets))
            modify $ delete secret
            pure $ RPBurn $ Right secret
        alreadyMinted minted fp = readSecretFile fp <&> (`elem` minted) . TokenName . snd . fromSecret bulletproofSetup

randomMintTerm :: MonadIO m => m EncoinsRequestTerm
randomMintTerm = randomRIO (1, 100) <&> RPMint . lovelaceOf

secretsToReqBody :: [(Secret, MintingPolarity)] -> ServerM EncoinsApi (InputOf EncoinsApi, TransactionUnspentOutputs)
secretsToReqBody (unzip -> (secrets, ps)) = do
    randomness <- randomIO
    networkId  <- getNetworkId
    walletAddr <- getWalletAddr
    ledgerAddr <- head <$> getTrackedAddresses
    utxos      <- getWalletUtxos
    let outputs = fromMaybe [] . toCSL . (,networkId) $ Map.toList utxos
        par = (ledgerAddr, walletAddr) :: TxParams
        bp   = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
        (v, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        signature  = ""
    pure (((par, (v, inputs), proof, signature), LedgerMode), outputs)

termsToSecrets :: [EncoinsRequestTerm] -> ServerM EncoinsApi [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> (, Burn) <$> readSecretFile fp

getWalletEncoinsTokens :: ServerM EncoinsApi [TokenName]
getWalletEncoinsTokens = do
    (_, refBeacon) <- getAuxillaryEnv
    let encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
        filterCS cs tokenName = if cs == encoinsSymb then Just tokenName else Nothing
    concatMap PAM.keys . PAM.elems . PAM.mapMaybeWithKey filterCS . getValue <$> getWalletValue

readSecretFile :: String -> IO Secret
readSecretFile s = readFile ("secrets/" <> s) <&> Secret (stringToFe s) . stringToFe

processFile :: MonadIO m => (Secret, MintingPolarity) -> m ()
processFile = \case
    (Secret g v, Mint) -> liftIO $ writeFile ("secrets/" <> feToString g) $ feToString v
    (Secret g _, Burn) -> liftIO $ removeFile $ "secrets/" <> feToString g

stringToFe :: FiniteField c => String -> Field c
stringToFe s = toFieldElement $ fromMaybe (error s) $ readMaybe s

feToString ::  FiniteField c => Field c -> String
feToString = show . fromFieldElement