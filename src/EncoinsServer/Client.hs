
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
import           Cardano.Server.Tx              (mkWalletTxOutRefs)
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
import           ENCOINS.Core.OnChain           (EncoinsRedeemer, TxParams, beaconCurrencySymbol, encoinsSymbol)
import           ENCOINS.Crypto.Field           (Field, FiniteField, fromFieldElement, toFieldElement)
import           EncoinsServer.Opts             (EncoinsRequestTerm (..), readTerms)
import           EncoinsServer.Server           (EncoinsApi, bulletproofSetup, getTrackedAddresses, verifierPKH)
import           Ledger.Ada                     (Ada (getLovelace), lovelaceOf)
import           Plutus.V1.Ledger.Api           (TokenName (TokenName), Value (getValue), fromBuiltin)
import           PlutusAppsExtra.IO.Wallet      (getWalletAddr, getWalletUtxos, getWalletValue)
import qualified PlutusTx.AssocMap              as PAM
import           PlutusTx.Builtins              (sha2_256)
import           PlutusTx.Extra.ByteString      (ToBuiltinByteString (..))
import           Servant.Client                 (runClientM)
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

autoTxClient :: forall (e :: ServerEndpoint).
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (EncoinsRedeemer, TransactionUnspentOutputs)
    , HasServantClientEnv
    ) => Interval -> ServerM EncoinsApi (Proxy e)
autoTxClient i = forever $ do
    genTerms >>= txClient @e
    waitTime =<< randomRIO (1, i * 2)

genTerms :: ServerM EncoinsApi [EncoinsRequestTerm]
genTerms = do
    (_, refBeacon) <- getAuxillaryEnv
    let encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
    minted  <- concatMap PAM.keys . PAM.elems . PAM.mapMaybeWithKey (filterCS encoinsSymb) . getValue <$> getWalletValue
    secrets <- liftIO $ listDirectory "secrets" >>= filterM (alreadyMinted minted)
    l       <- randomRIO (1, 5)
    (`evalStateT` secrets) $ replicateM l genTerm
    where
        genTerm = get >>= \secrets -> ifM ((null secrets ||) <$> randomIO) randomMintTerm $ do
            secret <- (secrets!!) <$> (randomIO <&> (`mod` length secrets))
            modify $ delete secret
            pure $ RPBurn $ Right secret
        randomMintTerm = randomRIO (1, 100) <&> RPMint . lovelaceOf
        alreadyMinted vs fp = readSecretFile fp <&> (`elem` vs) . TokenName . snd . fromSecret bulletproofSetup
        filterCS encoinsCS cs tokenName = if cs == encoinsCS then Just tokenName else Nothing

manualTxClient :: forall (e :: ServerEndpoint).
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (EncoinsRedeemer, TransactionUnspentOutputs)
    , HasServantClientEnv
    ) =>  Text -> ServerM EncoinsApi (Proxy (e :: ServerEndpoint))
manualTxClient txt = Proxy <$ txClient @e (fromMaybe (error "Unparsable input.") $ readTerms txt)

txClient :: forall (e :: ServerEndpoint).
    ( ClientEndpoint e EncoinsApi
    , EndpointArg e EncoinsApi ~ (EncoinsRedeemer, TransactionUnspentOutputs)
    , HasServantClientEnv
    ) => [EncoinsRequestTerm] -> ServerM EncoinsApi (Proxy (e :: ServerEndpoint))
txClient terms = Proxy <$ do
        mkWalletTxOutRefs <$> getWalletAddr <*> pure 10
        secrets <- termsToSecrets terms
        reqBody@((_,(v, inputs),_,_),_) <- secretsToReqBody secrets
        logMsg $ "Sending request with:\n" 
            <> foldl prettyInput "" (zip (map fst secrets) inputs) 
            <> "\n= " 
            <> T.pack (show v)
        res <- liftIO (flip runClientM ?servantClientEnv $ endpointClient @e @EncoinsApi reqBody)
        logMsg $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
        when (isRight res) $ mapM_ processFiles secrets
    where
        processFiles = \case
            (Secret g v, Mint) -> liftIO $ writeFile ("secrets/" <> feToString g) $ feToString v
            (Secret g _, Burn) -> liftIO $ removeFile $ "secrets/" <> feToString g
        feToString = show . fromFieldElement
        prettyInput acc (Secret _ v,(bbs, p)) = mconcat
            [ acc
            , "\n"
            , showPolarity p
            , T.pack (feToString v)
            , " "
            , encodeHex (fromBuiltin bbs)
            ]
        showPolarity = \case {Mint -> "M "; Burn -> "B "}

termsToSecrets :: [EncoinsRequestTerm] -> ServerM EncoinsApi [(Secret, MintingPolarity)]
termsToSecrets terms = do
        gammas <- replicateM (length terms) randomIO
        liftIO $ zipWithM toSecret gammas terms
    where
        toSecret g = \case
            RPMint a          -> pure $ (, Mint) $ Secret g $ toFieldElement $ getLovelace a
            RPBurn (Left s)   -> pure $ (, Burn) s
            RPBurn (Right fp) -> (, Burn) <$> readSecretFile fp

readSecretFile :: String -> IO Secret
readSecretFile s = readFile ("secrets/" <> s) <&> Secret (stringToFe s) . stringToFe

stringToFe :: FiniteField c => String -> Field c
stringToFe s = toFieldElement $ fromMaybe (error s) $ readMaybe s

secretsToReqBody :: [(Secret, MintingPolarity)] -> ServerM EncoinsApi (EncoinsRedeemer, TransactionUnspentOutputs)
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
    pure ((par, (v, inputs), proof, signature), outputs)