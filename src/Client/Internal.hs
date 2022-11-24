{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}

module Client.Internal where

import           Client.Opts
import           Control.Monad.Extra             (whenM)
import           Control.Monad.Reader
import           Utils.Logger                    (HasLogger(..), logSmth)
import           Data.Aeson                      (decode, FromJSON, eitherDecode)
import           Data.Aeson.Text                 (encodeToLazyText)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Maybe                      (fromJust)
import           Data.String                     (IsString(fromString))
import qualified Data.Text                       as T
import           Data.Text.Lazy.IO               as T
import           ENCOINS.Core.BaseTypes
import           ENCOINS.Core.Bulletproofs.Types (Inputs, Input(..), Secret(..), Proof(..))
import           ENCOINS.Core.Bulletproofs.Prove (fromSecret)
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, encoinsSymbol, stakingValidatorAddress)
import           ENCOINS.Core.OnChain            (EncoinsRedeemer, bulletproofSetup)
import           IO.Time                         (currentTime)
import           IO.Wallet                       (HasWallet(..), RestoreWallet, getWalletKeyHashes)
import           Ledger                          (TxOutRef (..), unPaymentPubKeyHash)
import           Ledger.Ada                      (Ada(..), lovelaceOf)
import           PlutusTx.Builtins.Class         (stringToBuiltinByteString, FromBuiltin (fromBuiltin))
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import           System.Directory                (doesFileExist, getDirectoryContents,
                                                  listDirectory, removeFile)
import           System.Random                   (randomRIO, randomIO)

newtype ClientM a = ClientM { unClientM :: ReaderT Env IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runClientM :: Env -> ClientM a -> IO a
runClientM env = flip runReaderT env . unClientM

data Env = Env
    { envBeaconRef :: TxOutRef
    , envWallet    :: RestoreWallet
    }

instance HasLogger ClientM where
    loggerFilePath = "client.log"

instance HasWallet ClientM where
    getRestoreWallet = asks envWallet

mkRedeemer :: Inputs -> Ada -> ClientM EncoinsRedeemer
mkRedeemer inputs val = do
    ct             <- liftIO currentTime
    (walletPKH, _) <- getWalletKeyHashes
    beaconRef <- liftIO $ decodeOrErrorFromFile "testnet/txOutRef.json"
    let txParams = ( getLovelace val
                   , stakingValidatorAddress $ encoinsSymbol $ beaconCurrencySymbol beaconRef
                   , unPaymentPubKeyHash walletPKH
                    , (0, ct + 3_600_000)
                   )
        dummyFE    = toFieldElement 200
        dummyGE    = fromJust $ toGroupElement $ fromString "aaaa"
        dummyProof = Proof dummyGE dummyGE dummyGE dummyGE dummyFE dummyFE dummyFE [dummyFE] [dummyFE]
    pure (txParams, inputs, dummyProof)
    where
    decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
    decodeOrErrorFromFile =  fmap (either error id . eitherDecode  . LBS.fromStrict) . BS.readFile

processPiece :: RequestPiece -> ClientM (Maybe (IO (), Ada, Input))
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

genGroupElement :: ClientM (FilePath, GroupElement)
genGroupElement = do
    content <- liftIO $ getDirectoryContents "secrets"
    len <- randomRIO (4, 8)
    let chars = ['0'..'9'] <> ['a'..'f']
    str <- replicateM len $ (chars !!) <$> randomRIO (0, length chars - 1)
    let fn = take 4 str <> ".json"
        bbs = stringToBuiltinByteString str
    case (fn `elem` content,  toGroupElement bbs) of
        (False, Just ge) -> pure (fn, ge)
        _                -> genGroupElement

genRequestPiece :: IO RequestPiece
genRequestPiece = randomIO >>= \case
    True  -> genMint
    False -> listDirectory "secrets" >>= \case
        [] -> genMint
        fs ->  RPBurn . (fs!!) <$> randomRIO (0, length fs - 1)
  where
    genMint = RPMint . Lovelace <$> randomRIO (1, 10_000_000)