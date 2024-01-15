{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Relay.Apps.Ipfs.Client
import           Encoins.Relay.Apps.Ipfs.Config
import           Encoins.Relay.Apps.Ipfs.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro  (AssetMintsAndBurnsData (..),
                                                 AssetMintsAndBurnsResponse (..))

import           Cardano.Api                    (NetworkId (..),
                                                 NetworkMagic (..))
import           Control.Concurrent             (threadDelay)
-- import           Control.Concurrent.Async       (withAsync)
import           Control.Exception.Safe         (Exception, SomeException,
                                                 catchAny, throwM, toException,
                                                 tryAny)
import           Control.Monad                  (forever, void)
import           Control.Monad.Extra            (forM_, mapMaybeM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (MonadReader (ask),
                                                 ReaderT (..), asks)
import           Data.Aeson                     (eitherDecodeFileStrict',
                                                 encodeFile)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC8
import           Data.List                      (sortOn)
import           Data.List.Extra                (unsnoc)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime,
                                                 posixDayLength,
                                                 utcTimeToPOSIXSeconds)
-- import           Data.Time.Format
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Say
import           Servant
-- import           Servant.Client
import           System.Directory               (createDirectoryIfMissing,
                                                 removeFile)
import           System.Directory.Extra         (listFiles)
import           System.FilePath.Posix          (takeFileName, (<.>), (</>))

type ServerIpfsApi =
         "minted" :> ReqBody '[JSON] TokenToCloud
                  :> Post '[JSON] CloudResponse
    -- :<|> "burned" :> ReqBody '[JSON] TokenToCloud :> Post '[JSON] Text

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = minted
          --  :<|> burned

handlerServer :: IpfsEnv -> ServerT ServerIpfsApi Handler
handlerServer env = hoistServer serverIpfsApiProxy (liftIO . runIpfsMonad env) serverIpfsApi

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }

app :: IpfsEnv -> Application
app = corsWithContentType . serve serverIpfsApiProxy . handlerServer

ipfsServer :: IO ()
ipfsServer = do
  env <- getIpfsEnv
  -- withAsync (rottenTokenHandler env) $ \_ -> do
  withRecovery "server" $
      run (envPort env) $ app env

-- TODO: get rid of liftIO

minted :: TokenToCloud -> IpfsMonad CloudResponse
minted t = do
  say "Minted token received"
  sayShow t
  -- print t
  eAssets <- liftIO $ tryAny $ getAssetMintsAndBurns
    (Testnet $ NetworkMagic 1)
    "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
    (fromString $ T.unpack $ tcAssetName t)
  sayShow eAssets
  case eAssets of
    Left err -> do
      sayShow err
      pure $ PinError $ T.pack $ show err
    Right assets -> case ambrAmount <$> getAsset assets of
      Left err -> do
        say err
        pure $ PinError err
      Right x
        | x > 0 -> do
            res <- pinJsonRequest $ mkTokentoIpfs t
            liftIO $ print res
            pure TokenPinned
        | otherwise -> do
            say "Token not found in blockchain. Thus it is not saved to ipfs"
            pure TokenBurned

getAsset :: [AssetMintsAndBurnsResponse] -> Either Text AssetMintsAndBurnsData
getAsset res = do
  resUniq <- if length res == 1
        then Right $ head res
        else Left "getAssetMintsAndBurns returned more than one asset."
  -- Sorting by slot is just in case
  let asset = sortOn ambrSlot $ ambrData resUniq
  let assetSorted = snd <$> (unsnoc asset)
  case assetSorted of
    Nothing -> Left "AssetMintsAndBurnsResponse has more than one data (page)"
    Just a  -> Right a

burned :: TokenToCloud -> IpfsMonad Text
burned t = do
  say "Burned token received"
  -- sayShow t
  let assetName = tcAssetName t
  res <- liftIO $ getAssetMintsAndBurns
    (Testnet $ NetworkMagic 1)
    "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
    (fromString $ T.unpack assetName)
  case getAsset res of
    Left err -> do
      say err
      pure err
    Right asset
      | ambrAmount asset < 0 -> do
      -- | True -> do -- TODO: remove the debug
          env <- ask
          let burnDir = envScheduleDirectory env
          cips <- getBurnedCips assetName
          liftIO $ print cips
          putInQueue burnDir assetName (ambrTimestamp asset) cips
          pure "Burned token put in queue for unpinning from ipfs"
      | otherwise -> do
          say "Token found on the blockchain. Thus it is should not be unpinned"
          pure "Not unpinned"



putInQueue :: FilePath -> Text -> UTCTime -> [Text] -> IpfsMonad ()
putInQueue scheduleDir assetName burnedTime cips = do
  let burnedTimePosix = utcTimeToPOSIXSeconds burnedTime
  let halfDay = posixDayLength / 2
  let removeTime = burnedTimePosix + halfDay
  liftIO $ createDirectoryIfMissing False scheduleDir
  mapM_ (\cip -> liftIO $ encodeFile (scheduleDir </> T.unpack cip <.> "json") $ MkRottenToken assetName removeTime cip) cips

getBurnedCips :: Text -> IpfsMonad [Text]
getBurnedCips assetName = do
  eFiles <- fetchMetaByStatusAndNameRequest "pinned" assetName
  case eFiles of
    Left err -> do
      liftIO
        $ putStrLn
        $ "fetchMetaByStatusAndNameRequest error: " <> show err
      pure []
    Right ((map ipfsPinHash . rows) -> cips) -> pure cips


removeRottenTokens :: IpfsMonad ()
removeRottenTokens  = do
  burnedDirectory <- asks envScheduleDirectory
  queueFiles <- liftIO $ listFiles burnedDirectory
  now <- liftIO $ getPOSIXTime
  cips <- liftIO $ mapMaybeM (selectRottenCip now) queueFiles
  sayShow cips
  res <- throwM ThisException
  sayShow @_ @MyException res
  forM_ cips $ \(cip, path)-> do
    eUnpined <- unpinByCipRequest cip
    sayShow eUnpined
    case eUnpined of
      Left err ->
        sayShow err
      Right _ -> do
        say "Burned token unpinned"
        eRemoved <- liftIO $ tryAny $ removeFile path
        case eRemoved of
          Left err -> do
            say "Fail to remove file"
            sayErrShow err
          Right _ -> say "Burned filed removed from queue"
  pure ()

selectRottenCip :: POSIXTime -> FilePath -> IO (Maybe (Text, FilePath))
selectRottenCip now tokenPath = do
  eRotten <- liftIO $ eitherDecodeFileStrict' tokenPath
  case eRotten of
    Left err -> do
      sayString err
      pure Nothing
    Right rotten
      | rtRemoveTime rotten <= now -> pure $ Just $ (rtCip rotten, tokenPath)
      | otherwise -> pure Nothing


rottenTokenHandler :: IpfsEnv -> IO ()
-- rottenTokenHandler env = withRecovery "removeRottenTokens" $ forever $ do
rottenTokenHandler env = forever $ do
      say "run removeRottenTokens"
      sayShow =<< getCurrentTime
      sayShow =<< getPOSIXTime
      runReaderT (unIpfsMonad removeRottenTokens) env
      -- Sleep for 1 hour (in microseconds)
      -- liftIO $ threadDelay $ 60 * 60 * 1000000
      threadDelay $ 60 * 1000000

withRecovery :: String -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      sayString $ "Exception caught in " <> nameOfAction <> ": " <> show e
      liftIO $ threadDelay $ 5 * 1000000
      withRecovery nameOfAction action


data MyException = ThisException | ThatException
    deriving Show

instance Exception MyException
