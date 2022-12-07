{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

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
import           ENCOINS.Core.BaseTypes          (MintingPolarity(..), toGroupElement, toFieldElement)
import           ENCOINS.Core.Bulletproofs.Types (Input(..), Secret(..), Proof(..))
import           ENCOINS.Core.Bulletproofs.Prove (fromSecret)
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, beaconMintTx, beaconSendTx, encoinsSymbol, encoinsTx, stakingValidatorAddress)
import           ENCOINS.Core.OnChain            (EncoinsRedeemer, EncoinsRedeemer, bulletproofSetup)
import           EncoinsServer.Opts              (ServerMode(..), runWithOpts, burnParser, mintParser, EncoinsRequestPiece(..), LovelaceM)
import           EncoinsServer.Setup             (runSetupM)
import           IO.Time                         (currentTime)
import           IO.Wallet                       (HasWallet(..), getWalletAddr, getWalletKeyHashes)
import           Ledger                          (TxOutRef, TxOutRef, unPaymentPubKeyHash)
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           PlutusTx.Builtins.Class         (FromBuiltin (fromBuiltin))
import           Servant                         (NoContent)
import           Server.Config                   (decodeOrErrorFromFile)
import           Server.Endpoints.Mint           (HasMintEndpoint(..))
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
            runSetupM env setupServer

setupServer :: ( MonadReader (Server.Env EncoinsServer) m
               , HasWallet m
               , HasLogger m
               ) => m ()
setupServer = void $ do
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

    processTokens red = void $ do
        bcs <- getCurrencySymbol
        mkTx [stakingValidatorAddress $ encoinsSymbol bcs] [encoinsTx bcs red]

instance HasMintEndpoint EncoinsServer where

    type MintApiResultOf EncoinsServer = '[NoContent]

    data MintErrorOf EncoinsServer
        deriving (Show, Exception)

    checkForMintErros _ = pure ()

    mintErrorHanlder = \case

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

mkEncoinsRedeemer :: (IO (), LovelaceM, [Input])
                  -> ClientM EncoinsServer (ClientM EncoinsServer (), RedeemerOf EncoinsServer)
mkEncoinsRedeemer (fileWork, val, inputs) = do
    ct             <- liftIO currentTime
    (walletPKH, _) <- getWalletKeyHashes
    beaconRef      <- asks envAuxiliary
    let txParams = ( getLovelace val
                   , stakingValidatorAddress $ encoinsSymbol $ beaconCurrencySymbol beaconRef
                   , unPaymentPubKeyHash walletPKH
                    , (0, ct + 3_600_000)
                   )
        dummyFE    = toFieldElement 200
        dummyGE    = fromJust $ toGroupElement $ fromString "aaaa"
        dummyProof = Proof dummyGE dummyGE dummyGE dummyGE dummyFE dummyFE dummyFE [dummyFE] [dummyFE]
    pure (liftIO fileWork, (txParams, inputs, dummyProof))

processPieces :: [EncoinsRequestPiece] -> ClientM EncoinsServer (IO (), LovelaceM, [Input])
processPieces cReq = sequence . catMaybes <$> traverse processPiece cReq

processPiece :: RequestPieceOf EncoinsServer -> ClientM s (Maybe (IO (), LovelaceM, Input))
processPiece (RPMint ada) = do
    sGamma <- liftIO randomIO
    let secret = Secret sGamma (toFieldElement $ getLovelace ada)
        bs = snd $ fromSecret bulletproofSetup secret
        file = T.unpack $ encodeByteString $ fromBuiltin bs
        filework = T.writeFile ("secrets/" <> file) $ encodeToLazyText secret
    pure $ Just (filework, ada, Input bs Mint)

processPiece (RPBurn file) = do
    let path = "secrets/" <> file
    fileExists <- liftIO $ (file `elem`) <$> getDirectoryContents "secrets"
    if fileExists
    then do
        secret <- liftIO $ fromJust . decode <$> LBS.readFile path
        let (val, bs) = fromSecret bulletproofSetup secret
            filework = whenM (doesFileExist path) $ removeFile path
        logSmth $ Input bs Burn
        pure $ Just (filework, (-1) * lovelaceOf val, Input bs Burn)
    else do
        logMsg $ "File " <> T.pack path <> " doesn't exists."
        pure Nothing