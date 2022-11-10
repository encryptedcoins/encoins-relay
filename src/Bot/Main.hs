{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Bot.Main where

import           Bot.Opts                        (runWithOpts, Options(..), BotMode(..), AutoOptions(..), BotRequest,
                                                  RequestPiece(..), Maximum)
import           Control.Monad.Extra             (whenM)
import           Control.Monad.Reader
import           Common.Logger                   (HasLogger(..), (.<), logSmth)
import           Common.Wait                     (waitTime)
import           Data.Aeson                      (encode, decode, FromJSON, eitherDecode)
import           Data.Aeson.Text                 (encodeToLazyText)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Functor                    ((<&>))
import           Data.List                       (nub)
import           Data.Maybe                      (catMaybes, fromJust)
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
import           Network.HTTP.Client             (httpLbs, defaultManagerSettings, newManager, parseRequest,
                                                  Manager, Request(..), RequestBody(..), responseStatus)
import           Network.HTTP.Types.Header       (hContentType)
import           Network.HTTP.Types.Status       (status204)
import           PlutusTx.Builtins.Class         (stringToBuiltinByteString, FromBuiltin (fromBuiltin))
import           Plutus.V1.Ledger.Bytes          (encodeByteString)
import qualified Server.Config                   as Server
import           System.Directory                (createDirectoryIfMissing, doesFileExist, getDirectoryContents,
                                                  listDirectory, removeFile)
import           System.Random                   (randomRIO, randomIO)


main :: IO ()
main = do
    Options{..}       <- runWithOpts
    Server.Config{..} <- Server.loadConfig
    createDirectoryIfMissing False "secrets"
    let fullAddress = "http://"
                   <> T.unpack confServerAddress
                   <> "/relayRequestMint"
    nakedRequest <- parseRequest fullAddress
    manager <- newManager defaultManagerSettings
    let env = Env confBeaconTxOutRef confWallet
        mkRequest' = mkRequest nakedRequest manager
    runBotM env $ logMsg "Starting bot..." >> case mode of
        Manual br            -> mkRequest' br
        Auto AutoOptions{..} -> forever $ do
            br <- genRequest maxTokensInReq
            mkRequest' br
            waitTime =<< randomRIO (1, averageRequestInterval * 2)

newtype BotM a = BotM { unBotM :: ReaderT Env IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runBotM :: Env -> BotM a -> IO a
runBotM env = flip runReaderT env . unBotM

data Env = Env
    { envBeaconRef :: TxOutRef
    , envWallet    :: RestoreWallet
    }

instance HasLogger BotM where
    loggerFilePath = "bot.log"

instance HasWallet BotM where
    getRestoreWallet = asks envWallet

mkRequest :: Request -> Manager -> BotRequest -> BotM ()
mkRequest nakedReq manager botReq = do
    logMsg $ "New tokens to send:\n" .< botReq
    (fileWork, val, inputs)  <- sequence . catMaybes <$> traverse processPiece botReq
    body <- RequestBodyLBS . encode <$> mkRedeemer inputs val
    let req = nakedReq
            { method = "POST"
            , requestBody = body
            , requestHeaders = [(hContentType, "application/json")]
            }
    resp <- liftIO $ httpLbs req manager
    logMsg $ "Received response:" .< resp
    when (successful resp) $ liftIO fileWork
  where
    successful = (== status204) . responseStatus

mkRedeemer :: Inputs -> Ada -> BotM EncoinsRedeemer
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

processPiece :: RequestPiece -> BotM (Maybe (IO (), Ada, Input))
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

genGroupElement :: BotM (FilePath, GroupElement)
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

genRequest :: MonadIO m => Maximum -> m BotRequest
genRequest ub = liftIO $ randomRIO (1, ub) >>= flip replicateM genRequestPiece <&> nub

genRequestPiece :: IO RequestPiece
genRequestPiece = randomIO >>= \case
    True  -> genMint
    False -> listDirectory "secrets" >>= \case
        [] -> genMint
        fs ->  RPBurn . (fs!!) <$> randomRIO (0, length fs - 1)
  where
    genMint = RPMint . Lovelace <$> randomRIO (1, 10_000_000)