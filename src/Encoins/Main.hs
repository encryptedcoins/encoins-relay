{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Encoins.Main (Encoins) where

import           Client.Internal                 (ClientM, ClientRequestOf, HasClient(..), envAuxiliary)
import           Control.Applicative             ((<|>), Alternative (..))
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
import           Encoins.Opts                    (burnParser, mintParser, EncoinsRequestPiece(..))
import           IO.Time                         (currentTime)
import           IO.Wallet                       (getWalletKeyHashes)
import           Ledger                          (TxOutRef, TxOutRef, unPaymentPubKeyHash)
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           PlutusTx.Builtins.Class         (FromBuiltin (fromBuiltin))
import qualified Server.Internal                 as Server
import           Server.Internal                 (decodeOrErrorFromFile, Config(..), HasServer(..))
import           Server.Tx                       (mkTx)
import           System.Directory                (listDirectory, doesFileExist, getDirectoryContents, listDirectory, removeFile)
import           System.Random                   (randomIO, randomRIO, randomRIO, randomIO)

data Encoins

instance HasServer Encoins where

    type AuxiliaryEnvOf Encoins = TxOutRef

    loadAuxiliaryEnv = decodeOrErrorFromFile

    type RedeemerOf Encoins = EncoinsRedeemer

    getCurrencySymbol = asks $ beaconCurrencySymbol . Server.envAuxiliary

    processTokens red = do
        params <- getCurrencySymbol
        mkTx [encoinsTx params red]

    setupServer Config{..} = mkTx
        [ beaconMintTx confAuxiliaryEnv
        , beaconSendTx confAuxiliaryEnv
        ]

instance HasClient Encoins where

    type RequestPieceOf Encoins = EncoinsRequestPiece

    genRequestPiece = genEncoinsRequestPiece

    type RequestPieceOf Encoins = EncoinsRequestPiece

    parseRequestPiece = mintParser <|> burnParser

    mkRedeemer = mkEncoinsRedeemer

genEncoinsRequestPiece :: IO EncoinsRequestPiece
genEncoinsRequestPiece = randomIO >>= \case
    True  -> genMint
    False -> listDirectory "secrets" >>= \case
        [] -> genMint
        fs ->  RPBurn . (fs!!) <$> randomRIO (0, length fs - 1)
  where
    genMint = RPMint . fromInteger <$> randomRIO (1, 10_000_000)

mkEncoinsRedeemer :: ClientRequestOf Encoins -> ClientM Encoins (ClientM Encoins (), RedeemerOf Encoins)
mkEncoinsRedeemer cReq = do
    (fileWork, val, inputs)  <- sequence . catMaybes <$> traverse processPiece cReq
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

processPiece :: RequestPieceOf Encoins -> ClientM s (Maybe (IO (), Ada, Input))
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