{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module EncoinsServer.Main (EncoinsServer, mkEncoinsRedeemer) where

import           Client.Internal                 (ClientM, HasClient(..), envAuxiliary)
import           Control.Applicative             (Alternative (..))
import           Control.Monad                   ((>=>), void)
import           Control.Monad.Catch             (Exception)
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Text                       as T
import           Control.Monad.Extra             (whenM)
import           Control.Monad.Reader            (MonadIO(..), asks)
import           Utils.Logger                    (HasLogger(..), logSmth)
import           Data.Aeson                      (decode)
import           Data.Aeson.Text                 (encodeToLazyText)
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.String                     (IsString(fromString))
import qualified Data.Text.Lazy.IO               as T
import           ENCOINS.Core.BaseTypes          (MintingPolarity(..), toGroupElement, toFieldElement)
import           ENCOINS.Core.Bulletproofs.Types (Input(..), Secret(..), Proof(..))
import           ENCOINS.Core.Bulletproofs.Prove (fromSecret)
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, beaconMintTx, beaconSendTx, encoinsSymbol, encoinsTx, stakingValidatorAddress)
import           ENCOINS.Core.OnChain            (EncoinsRedeemer, EncoinsRedeemer, bulletproofSetup)
import           EncoinsServer.Opts              (burnParser, mintParser, EncoinsRequestPiece(..))
import           IO.Time                         (currentTime)
import           IO.Wallet                       (getWalletKeyHashes, getWalletAddr)
import           Ledger                          (TxOutRef, TxOutRef, unPaymentPubKeyHash)
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           PlutusTx.Builtins.Class         (FromBuiltin (fromBuiltin))
import           Servant                         (NoContent)
import           Server.Endpoints.Mint           (HasMintEndpoint(..))
import qualified Server.Internal                 as Server
import           Server.Internal                 (decodeOrErrorFromFile, Config(..), HasServer(..))
import           Server.Tx                       (mkTx)
import           System.Directory                (listDirectory, doesFileExist, getDirectoryContents, listDirectory, removeFile)
import           System.Random                   (randomIO, randomRIO, randomRIO, randomIO)

data EncoinsServer

instance HasServer EncoinsServer where

    type AuxiliaryEnvOf EncoinsServer = TxOutRef

    loadAuxiliaryEnv = decodeOrErrorFromFile

    type RedeemerOf EncoinsServer = EncoinsRedeemer

    getCurrencySymbol = asks $ beaconCurrencySymbol . Server.envAuxiliary

    processTokens red = do
        bcs <- getCurrencySymbol
        void $ mkTx [stakingValidatorAddress $ encoinsSymbol bcs] [encoinsTx bcs red]

    setupServer Config{..} = do
        walletAddr <- getWalletAddr
        void $ mkTx [walletAddr]
            [ beaconMintTx confAuxiliaryEnv
            , beaconSendTx confAuxiliaryEnv
            ]

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

mkEncoinsRedeemer :: (IO (), Ada, [Input])
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

processPieces :: [EncoinsRequestPiece] -> ClientM EncoinsServer (IO (), Ada, [Input])
processPieces cReq = sequence . catMaybes <$> traverse processPiece cReq

processPiece :: RequestPieceOf EncoinsServer -> ClientM s (Maybe (IO (), Ada, Input))
processPiece (RPMint ada) = do
    secretGamma <- liftIO randomIO
    let secret = Secret secretGamma (toFieldElement $ getLovelace ada)
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