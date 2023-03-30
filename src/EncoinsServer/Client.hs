
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

module EncoinsServer.Client where

import           CSL                            (TransactionUnspentOutputs)
import           CSL.Class                      (ToCSL (..))
import           Cardano.Server.Client.Handle   (ClientHandle (..), HasServantClientEnv, autoWith, manualWith)
import           Cardano.Server.Client.Internal (ClientEndpoint, EndpointArg, Interval, ServerEndpoint, endpointClient)
import           Cardano.Server.Internal        (ServerM, getAuxillaryEnv, getNetworkId)
import           Cardano.Server.Utils.Logger    (logMsg, (.<))
import           Cardano.Server.Utils.Wait      (waitTime)
import           Control.Monad                  (zipWithM)
import           Control.Monad.Extra            (ifM)
import           Control.Monad.Reader           (MonadIO (..), forever, replicateM, when)
import           Control.Monad.State            (evalStateT, get, modify)
import           Data.Data                      (Proxy (Proxy))
import           Data.Default                   (def)
import           Data.Either                    (isRight)
import           Data.Functor                   ((<&>))
import           Data.List                      (delete)
import           Data.List.Extra                (dropPrefix)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           ENCOINS.BaseTypes              (MintingPolarity (Burn, Mint), groupIdentity)
import           ENCOINS.Bulletproofs           (BulletproofSetup (..), Secret (Secret), bulletproof, bulletproofM, bulletproofN,
                                                 fromSecret, parseBulletproofParams)
import           ENCOINS.Core.OnChain           (beaconCurrencySymbol, encoinsSymbol, stakingValidatorAddress)
import           ENCOINS.Core.V1.OffChain       (EncoinsRedeemerWithData)
import           ENCOINS.Crypto.Field           (fromFieldElement, toFieldElement)
import           EncoinsServer.Opts             (EncoinsRequestTerm (..))
import           EncoinsServer.Server           (EncoinsApi, verifierPKH)
import           Ledger.Ada                     (Ada (getLovelace))
import           PlutusAppsExtra.IO.Wallet      (getWalletAddr, getWalletUtxos)
import           PlutusTx.Extra.ByteString      (ToBuiltinByteString (..))
import           Servant.Client                 (runClientM)
import           System.Directory               (listDirectory, removeFile)
import           System.Random                  (randomIO, randomRIO)
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

readTerms :: Text -> Maybe [EncoinsRequestTerm]
readTerms = mapM (readTerm . T.unpack) . T.splitOn ","
    where
        readTerm = \case
            'b':fp  -> Just $ RPBurn $ Right $ "secrets/" <> fp
            'm':ada -> fmap (RPMint . fromInteger) . readMaybe $ ada
            _       -> Nothing

genTerms :: MonadIO m => m [EncoinsRequestTerm]
genTerms = liftIO $ do
    secrets <- listDirectory "secrets"
    l       <- randomRIO (1,10)
    flip evalStateT secrets $ replicateM l genTerm
    where
        genTerm = get >>= \secrets -> ifM ((null secrets ||) <$> randomIO) randomMintTerm $ do
            secret <- (secrets!!) <$> (randomIO <&> (`mod` length secrets))
            modify $ delete secret
            pure $ RPBurn $ Right $ "secrets/" <> secret
        randomMintTerm = randomIO >>= \case
            m@(RPMint _) -> pure m
            _            -> randomMintTerm

autoTxClient :: forall (e :: ServerEndpoint).
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (EncoinsRedeemerWithData, TransactionUnspentOutputs)
    , HasServantClientEnv
    ) => Interval -> ServerM EncoinsApi (Proxy e)
autoTxClient i = forever $ do
    genTerms >>= txClient @e
    waitTime =<< randomRIO (1, i * 2)

manualTxClient :: forall (e :: ServerEndpoint).
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (EncoinsRedeemerWithData, TransactionUnspentOutputs)
    , HasServantClientEnv
    ) =>  Text -> ServerM EncoinsApi (Proxy (e :: ServerEndpoint))
manualTxClient txt = Proxy <$ txClient @e (fromMaybe (error "Unparsable input.") $ readTerms txt)

txClient :: forall (e :: ServerEndpoint).
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (EncoinsRedeemerWithData, TransactionUnspentOutputs)
    , HasServantClientEnv
    ) => [EncoinsRequestTerm] -> ServerM EncoinsApi (Proxy (e :: ServerEndpoint))
txClient terms = Proxy <$ do
        secrets <- termsToSecrets terms
        reqBody <- secretsToInput secrets
        logMsg $ "Sending request with:\n" .< reqBody
        res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @EncoinsApi reqBody)
        logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
        when (isRight res) $ mapM_ prcessFiles secrets
    where
        prcessFiles = \case
            (Secret gamma v, Mint) -> liftIO $ writeFile ("secrets/" <> feToString gamma) $ feToString v
            (Secret gamma _, Burn) -> liftIO $ removeFile $ "secrets/" <> feToString gamma
        feToString = show . fromFieldElement

termsToSecrets :: [EncoinsRequestTerm] -> ServerM EncoinsApi [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> readFile fp <&> (, Burn) . Secret (stringToFe fp) . stringToFe
        stringToFe s = toFieldElement . fromMaybe (error s) . readMaybe . dropPrefix "secrets/" $ s

secretsToInput :: [(Secret, MintingPolarity)] -> ServerM EncoinsApi (EncoinsRedeemerWithData, TransactionUnspentOutputs)
secretsToInput (unzip -> (secrets, ps)) = do
    beaconRef  <- getAuxillaryEnv
    randomness <- randomIO
    netowrkId  <- getNetworkId
    outputs    <- fromMaybe [] . toCSL . (,netowrkId) . Map.toList <$> getWalletUtxos
    walletAddr <- getWalletAddr
    let scriptAddr = stakingValidatorAddress $ encoinsSymbol (beaconCurrencySymbol beaconRef, verifierPKH)
        par = parseBulletproofParams $ toBytes scriptAddr
        inputs = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
        (v, _, proof) = bulletproof bulletproofSetup par secrets ps randomness
        signature  = ""
        redAddr = if v > 0 then scriptAddr else walletAddr
    pure ((walletAddr, (redAddr, (v, inputs), proof, signature)), outputs)

bulletproofSetup :: BulletproofSetup
bulletproofSetup = BulletproofSetup ge ge ges ges
    where
        ge = groupIdentity -- fromJust $ toGroupElement "aaaa"
        ges = replicate (fromInteger $ bulletproofN * bulletproofM) ge