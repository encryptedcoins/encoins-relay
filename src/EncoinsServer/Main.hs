{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module EncoinsServer.Main where

import           Client.Class                    (ClientM, HasClient(..), envAuxiliary)
import           Control.Applicative             (Alternative (..))
import           Control.Monad                   ((>=>), void)
import           Control.Monad.Catch             (Exception)
import           Control.Monad.Extra             (whenM)
import           Control.Monad.IO.Class          (MonadIO(..))
import           Control.Monad.Reader            (MonadReader, asks)
import           Data.Aeson                      (decode)
import           Data.Aeson.Text                 (encodeToLazyText)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.String                     (IsString(fromString))
import qualified Data.Text                       as T
import qualified Data.Text.Lazy.IO               as T
import           ENCOINS.BaseTypes               (MintingPolarity(..), toGroupElement)
import           ENCOINS.Bulletproofs.Types      (Secret(..), Proof(..))
import           ENCOINS.Bulletproofs.Prove      (fromSecret)
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, beaconMintTx, beaconSendTx, encoinsSymbol, encoinsTx, stakingValidatorAddress)
import           ENCOINS.Core.V1.OffChain        (verifierPKH)
import           ENCOINS.Core.OnChain            (EncoinsRedeemer, EncoinsRedeemer, bulletproofSetup)
import           ENCOINS.Crypto.Field            (toFieldElement)
import           EncoinsServer.Opts              (ServerMode(..), runWithOpts, burnParser, mintParser, EncoinsRequestPiece(..), LovelaceM)
import           EncoinsServer.Setup             (runSetupM)
import           IO.ChainIndex                   (getWalletUtxos)
import           IO.Wallet                       (HasWallet(..))
import           Ledger                          (TxOutRef, TxOutRef)
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           Plutus.V2.Ledger.Api            (BuiltinByteString)
import           PlutusTx.Builtins.Class         (FromBuiltin (fromBuiltin))
import           Server.Config                   (decodeOrErrorFromFile)
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

    type InputOf EncoinsServer = EncoinsRedeemer

    serverTrackedAddresses = do
        bcs <- asks $ beaconCurrencySymbol . Server.envAuxiliary
        return [stakingValidatorAddress $ encoinsSymbol (bcs, verifierPKH)]

instance HasTxEndpoints EncoinsServer where

    type TxApiResultOf EncoinsServer = DefaultTxApiResult

    data TxEndpointsErrorOf EncoinsServer
        deriving (Show, Exception)
    
    txEndpointsTxBuilders red = do
        bcs <- asks $ beaconCurrencySymbol . Server.envAuxiliary
        pure [encoinsTx (bcs, verifierPKH) red]

    checkForTxEndpointsErrors _ = pure ()

    txEndpointsErrorHandler = \case

instance HasClient EncoinsServer where

    type RequestTermOf EncoinsServer = EncoinsRequestPiece

    genRequestTerm = genEncoinsRequestTerm

    parseRequestTerm = mintParser <|> burnParser

    makeServerInput = processTerms >=> mkEncoinsRedeemer

genEncoinsRequestTerm :: IO EncoinsRequestPiece
genEncoinsRequestTerm = randomIO >>= \case
        True  -> genMint
        False -> listDirectory "secrets" >>= \case
            [] -> genMint
            fs ->  RPBurn . (fs!!) <$> randomRIO (0, length fs - 1)
    where
        genMint = RPMint . fromInteger <$> randomRIO (1, 10)

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
    pure (liftIO fileWork, (txParams, input, dummyProof, signature))

processTerms :: [EncoinsRequestPiece]
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