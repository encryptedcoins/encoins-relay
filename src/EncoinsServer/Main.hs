{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module EncoinsServer.Main where

import           Client.Internal                 (ClientM, HasClient(..), envAuxiliary)
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
import           ENCOINS.Core.OnChain            (EncoinsRedeemer, EncoinsRedeemer, bulletproofSetup)
import           ENCOINS.Crypto.Field            (toFieldElement)
import           EncoinsServer.Opts              (ServerMode(..), runWithOpts, burnParser, mintParser, EncoinsRequestPiece(..), LovelaceM)
import           EncoinsServer.Setup             (runSetupM)
import           IO.Wallet                       (HasWallet(..), getWalletAddr)
import           Ledger                          (TxOutRef, TxOutRef)
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           Plutus.V2.Ledger.Api            (BuiltinByteString)
import           PlutusTx.Builtins.Class         (FromBuiltin (fromBuiltin))
import           Server.Config                   (decodeOrErrorFromFile)
import           Server.Endpoints.Tx.Internal    (HasTxEndpoints(..), DefaultTxApiResult)
import qualified Server.Internal                 as Server
import           Server.Internal                 (HasServer(..))
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
    walletAddr <- getWalletAddr
    mkTx [walletAddr] [beaconMintTx txOutRef]
    mkTx [walletAddr] [beaconSendTx txOutRef]

data EncoinsServer

instance HasServer EncoinsServer where

    type AuxiliaryEnvOf EncoinsServer = TxOutRef

    loadAuxiliaryEnv = decodeOrErrorFromFile

    type RedeemerOf EncoinsServer = EncoinsRedeemer

    getCurrencySymbol = asks $ beaconCurrencySymbol . Server.envAuxiliary

instance HasTxEndpoints EncoinsServer where

    type TxApiResultOf EncoinsServer = DefaultTxApiResult

    data TxEndpointsErrorOf EncoinsServer
        deriving (Show, Exception)

    checkForTxEndpointsErros _ = pure ()

    txEndpointsErrorHanlder = \case

    txEndpointsTxBuilders red = do
        bcs <- getCurrencySymbol
        pure [encoinsTx bcs red]

instance HasClient EncoinsServer where

    type RequestPieceOf EncoinsServer = EncoinsRequestPiece

    genRequestPiece = genEncoinsRequestPiece

    parseRequestPiece = mintParser <|> burnParser

    mkRedeemer = processPieces >=> mkEncoinsRedeemer

genEncoinsRequestPiece :: IO EncoinsRequestPiece
genEncoinsRequestPiece = randomIO >>= \case
        True  -> genMint
        False -> listDirectory "secrets" >>= \case
            [] -> genMint
            fs ->  RPBurn . (fs!!) <$> randomRIO (0, length fs - 1)
    where
        genMint = RPMint . fromInteger <$> randomRIO (1, 10)

mkEncoinsRedeemer :: (IO (), LovelaceM, [(BuiltinByteString, MintingPolarity)])
    -> ClientM EncoinsServer (ClientM EncoinsServer (), RedeemerOf EncoinsServer)
mkEncoinsRedeemer (fileWork, val, inputs) = do
    beaconRef      <- asks envAuxiliary
    let txParams   = stakingValidatorAddress $ encoinsSymbol $ beaconCurrencySymbol beaconRef
        input      = (getLovelace val, inputs) 
        dummyFE    = toFieldElement 200
        dummyGE    = fromJust $ toGroupElement $ fromString "aaaa"
        dummyProof = Proof dummyGE dummyGE dummyGE dummyGE dummyFE dummyFE [dummyFE] [dummyFE] dummyFE
        signature  = "" 
    pure (liftIO fileWork, (txParams, input, dummyProof, signature))

processPieces :: [EncoinsRequestPiece] 
    -> ClientM EncoinsServer (IO (), LovelaceM, [(BuiltinByteString, MintingPolarity)])
processPieces cReq = sequence . catMaybes <$> traverse processPiece cReq

processPiece :: RequestPieceOf EncoinsServer 
    -> ClientM s (Maybe (IO (), LovelaceM, (BuiltinByteString, MintingPolarity)))
processPiece (RPMint ada) = do
    sGamma <- liftIO randomIO
    let secret = Secret sGamma (toFieldElement $ getLovelace ada)
        bs = snd $ fromSecret bulletproofSetup secret
        file = T.unpack $ encodeByteString $ fromBuiltin bs
        filework = T.writeFile ("secrets/" <> file) $ encodeToLazyText secret
    logSmth bs
    pure $ Just (filework, ada, (bs, Mint))

processPiece (RPBurn file) = do
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