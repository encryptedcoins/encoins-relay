{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module EncoinsServer.Main where

import           Client.Class                    (ClientM, HasClient(..), envAuxiliary)
import           Control.Applicative             (Alternative (..))
import           Control.Exception               (throw)
import           Control.Monad                   ((>=>), void)
import           Control.Monad.Catch             (Exception)
import           Control.Monad.Extra             (whenM)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Control.Monad.Reader            (MonadReader, asks)
import           CSL                             (TransactionUnspentOutputs)
import           CSL.Class                       (fromCSL)
import           Data.Aeson                      (decode)
import           Data.Aeson.Text                 (encodeToLazyText)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Map                        (fromList)
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.String                     (IsString(fromString))
import qualified Data.Text                       as T
import qualified Data.Text.Lazy.IO               as T
import           ENCOINS.BaseTypes               (MintingPolarity(..), toGroupElement)
import           ENCOINS.Bulletproofs.Types      (Secret(..), Proof(..))
import           ENCOINS.Bulletproofs.Prove      (fromSecret)
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, beaconMintTx, beaconSendTx, encoinsSymbol, encoinsTx, stakingValidatorAddress)
import           ENCOINS.Core.V1.OffChain        (verifierPKH, EncoinsRedeemerWithData)
import           ENCOINS.Core.OnChain            (bulletproofSetup)
import           ENCOINS.Crypto.Field            (toFieldElement)
import           EncoinsServer.Opts              (ServerMode(..), EncoinsRequestTerm(..), LovelaceM, runWithOpts, burnParser, mintParser)
import           EncoinsServer.Setup             (runSetupM)
import           IO.ChainIndex                   (getWalletUtxos)
import           IO.Wallet                       (HasWallet(..))
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           Ledger                          (TxOutRef)
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           Plutus.V2.Ledger.Api            (BuiltinByteString)
import           PlutusTx.Builtins.Class         (FromBuiltin (fromBuiltin))
import           Server.Config                   (decodeOrErrorFromFile)
import           Server.Endpoints.Servant        (respondWithStatus)
import           Server.Endpoints.Tx.Class       (HasTxEndpoints(..), DefaultTxApiResult)
import qualified Server.Class                    as Server
import           Server.Class                    (HasServer(..))
import           Server.Main                     (runServer)
import           Server.Tx                       (mkTx)
import           System.Directory                (listDirectory, doesFileExist, getDirectoryContents, listDirectory, removeFile)
import           System.Random                   (randomIO, randomRIO, randomRIO, randomIO)
import           Utils.Logger                    (HasLogger(..), logSmth)

runEncoinsServer :: IO ()
runEncoinsServer = do
    mode <- runWithOpts
    case mode of
        Run   -> runServer @EncoinsServer
        Setup -> do
            env <- Server.loadEnv @EncoinsServer
            runSetupM env setupEncoinsServer

setupEncoinsServer :: ( MonadReader (Server.Env EncoinsServer) m
               , HasWallet m
               , HasLogger m
               ) => m ()
setupEncoinsServer = void $ do
    txOutRef <- asks Server.envAuxiliary
    utxos <- getWalletUtxos
    mkTx [] utxos [beaconMintTx txOutRef]
    utxos' <- getWalletUtxos
    mkTx [] utxos' [beaconSendTx txOutRef]

data EncoinsServer

instance HasServer EncoinsServer where

    type AuxiliaryEnvOf EncoinsServer = TxOutRef

    loadAuxiliaryEnv = decodeOrErrorFromFile

    type InputOf EncoinsServer = EncoinsRedeemerWithData

    serverTrackedAddresses = do
        bcs <- asks $ beaconCurrencySymbol . Server.envAuxiliary
        return [stakingValidatorAddress $ encoinsSymbol (bcs, verifierPKH)]

instance HasTxEndpoints EncoinsServer where
    type TxApiRequestOf EncoinsServer = (InputOf EncoinsServer, TransactionUnspentOutputs)

    type TxApiResultOf EncoinsServer = DefaultTxApiResult

    data TxEndpointsErrorOf EncoinsServer = CorruptedExternalUTXOs | IncorrectVerifierSignature
        deriving (Show, Exception)

    txEndpointsProcessRequest (redWithData, utxosCSL) = do
        let utxos = maybe (throw CorruptedExternalUTXOs) fromList $ fromCSL utxosCSL
        return (redWithData, utxos)

    txEndpointsTxBuilders red = do
        bcs <- asks $ beaconCurrencySymbol . Server.envAuxiliary
        pure [encoinsTx (bcs, verifierPKH) red]

    txEndpointsErrorHandler = \case
        CorruptedExternalUTXOs     -> respondWithStatus @422
            "The request contained corrupted external UTXO data."
        IncorrectVerifierSignature -> respondWithStatus @422
            "The request contained incorrect verifier signature."

instance HasClient EncoinsServer where

    type RequestTermOf EncoinsServer = EncoinsRequestTerm

    genRequestTerm = genEncoinsRequestTerm

    parseRequestTerm = mintParser <|> burnParser

    makeServerInput = processTerms >=> mkEncoinsRedeemer

-- TODO: this requires rework
genEncoinsRequestTerm :: IO EncoinsRequestTerm
genEncoinsRequestTerm = randomIO >>= \case
        True  -> genMint
        False -> listDirectory "secrets" >>= \case
            [] -> genMint
            fs ->  RPBurn . (fs!!) <$> randomRIO (0, length fs - 1)
    where
        genMint = RPMint . fromInteger <$> randomRIO (1, 10)

-- TODO: this requires rework
mkEncoinsRedeemer :: (IO (), LovelaceM, [(BuiltinByteString, MintingPolarity)])
    -> ClientM EncoinsServer (ClientM EncoinsServer (), InputOf EncoinsServer)
mkEncoinsRedeemer (fileWork, val, inputs) = do
    beaconRef      <- asks envAuxiliary
    let txParams   = stakingValidatorAddress $ encoinsSymbol (beaconCurrencySymbol beaconRef, verifierPKH)
        input      = (getLovelace val, inputs)
        dummyFE    = toFieldElement 200
        dummyGE    = fromJust $ toGroupElement $ fromString "aaaa"
        dummyProof = Proof dummyGE dummyGE dummyGE dummyGE dummyFE dummyFE [dummyFE] [dummyFE] dummyFE
        signature  = ""
    pure (liftIO fileWork, (txParams, (txParams, input, dummyProof, signature)))

processTerms :: [EncoinsRequestTerm]
    -> ClientM EncoinsServer (IO (), LovelaceM, [(BuiltinByteString, MintingPolarity)])
processTerms cReq = sequence . catMaybes <$> traverse processTerm cReq

processTerm :: RequestTermOf EncoinsServer
    -> ClientM s (Maybe (IO (), LovelaceM, (BuiltinByteString, MintingPolarity)))
processTerm (RPMint ada) = do
    sGamma <- liftIO randomIO
    let secret = Secret sGamma (toFieldElement $ getLovelace ada)
        bs = snd $ fromSecret bulletproofSetup secret
        file = T.unpack $ encodeByteString $ fromBuiltin bs
        filework = T.writeFile ("secrets/" <> file) $ encodeToLazyText secret
    logSmth bs
    pure $ Just (filework, ada, (bs, Mint))

processTerm (RPBurn file) = do
    let path = "secrets/" <> file
    fileExists <- liftIO $ (file `elem`) <$> getDirectoryContents "secrets"
    if fileExists
    then do
        secret <- liftIO $ fromJust . decode <$> LBS.readFile path
        let (val, bs) = fromSecret bulletproofSetup secret
            filework = whenM (doesFileExist path) $ removeFile path
        logSmth bs
        pure $ Just (filework, (-1) * lovelaceOf val, (bs, Burn))
    else do
        logMsg $ "File " <> T.pack path <> " doesn't exists."
        pure Nothing