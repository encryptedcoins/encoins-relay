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
import           Control.Concurrent.Async       (withAsync)
import           Control.Monad                  (forever)
import           Control.Monad.Extra            (forM_, mapMaybeM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (MonadReader (ask),
                                                 ReaderT (..))
import           Data.Aeson                     (eitherDecodeFileStrict',
                                                 encodeFile)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC8
import           Data.List.Extra                (unsnoc)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime)
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime,
                                                 posixDayLength,
                                                 utcTimeToPOSIXSeconds)
import           Data.Time.Format
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Servant
import           Servant.Client
import           System.Directory               (createDirectoryIfMissing)
import           System.Directory.Extra         (listFiles)
import           System.FilePath.Posix          (takeFileName, (<.>), (</>))

type ServerIpfsApi =
         "minted" :> ReqBody '[JSON] Token :> Post '[JSON] Text
    :<|> "burned" :> ReqBody '[JSON] Token :> Post '[JSON] Text

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = minted
           :<|> burned

handlerServer :: IpfsEnv -> ServerT ServerIpfsApi Handler
handlerServer env = hoistServer serverIpfsApiProxy (liftIO . flip runReaderT env) serverIpfsApi

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
  withAsync (rottenTokenHandler env) $ \_ ->
    run (envPort env) $ app env

-- TODO: get rid of liftIO

minted :: Token -> IpfsMonad Text
minted t = do
  liftIO $ putStrLn "Minted token received"
  liftIO $ print t
  let mAssetName = name $ pinataMetadata t
  case mAssetName of
    Nothing -> do
      liftIO $ print "No assetName received"
      pure "No assetName received"
    Just assetName -> do
      liftIO $ print assetName
      assets <- liftIO $ getAssetMintsAndBurns
        (Testnet $ NetworkMagic 1)
        "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
        (fromString $ T.unpack assetName)
      liftIO $ print assets
      case ambrAmount <$> getAsset assets of
        Nothing -> do
          liftIO $ putStrLn $ "Unexpected data"
          pure "Unexpected data"
        Just x
          | x > 0 -> do
              res <- pinJsonRequest t
              liftIO $ putStrLn "Minted token saved"
              liftIO $ print res
              file <- fetchMetaByStatusAndNameRequest "pinned" assetName
              liftIO $ print file
              pure "Minted token saved to ipfs"
          | otherwise -> do
              liftIO $ putStrLn "Token not found in blockchain. Thus it is not saved to ipfs"
              pure "Not saved"

getAsset :: [AssetMintsAndBurnsResponse] -> Maybe AssetMintsAndBurnsData
getAsset res =
  let resUniq = if length res == 1
        then snd <$> unsnoc res
        else Nothing
  in snd <$> (unsnoc . ambrData =<< resUniq)

burned :: Token -> IpfsMonad Text
burned t = do
  liftIO $ putStrLn "Burned token received"
  -- liftIO $ print t
  let mAssetName = name $ pinataMetadata t
  case mAssetName of
    Nothing -> do
      liftIO $ print "No assetName received"
      pure "No assetName received"
    Just assetName -> do
      -- liftIO $ print assetName
      res <- liftIO $ getAssetMintsAndBurns
        (Testnet $ NetworkMagic 1)
        "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
        (fromString $ T.unpack assetName)
      case getAsset res of
        Nothing -> do
          liftIO $ putStrLn $ "Unexpected asset data"
          pure "Unexpected asset data"
        Just asset
          | ambrAmount asset < 0 -> do
              env <- ask
              let burnDir = envScheduleDirectory env
              cips <- getBurnedCips assetName
              liftIO $ print cips
              liftIO $ putInQueue burnDir assetName (ambrTimestamp asset) cips
              pure "Burned token put in queue for unpinning from ipfs"
          | otherwise -> do
              liftIO $ putStrLn "Token found on the blockchain. Thus it is should not be unpinned"
              pure "Not unpinned"



putInQueue :: FilePath -> Text -> UTCTime -> [Text] -> IO ()
putInQueue scheduleDir assetName burnedTime cips = do
  let burnedTimePosix = utcTimeToPOSIXSeconds burnedTime
  let halfDay = posixDayLength / 2
  let removeTime = burnedTimePosix + halfDay
  createDirectoryIfMissing False scheduleDir
  mapM_ (\cip -> encodeFile (scheduleDir </> T.unpack cip <.> "json") $ MkRottenToken assetName removeTime cip) cips

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


removeRottenTokens :: FilePath -> IpfsMonad ()
removeRottenTokens burnedDirectory = do
  queueFiles <- liftIO $ listFiles burnedDirectory
  now <- liftIO $ getPOSIXTime
  cips <- liftIO $ mapMaybeM (selectRottenCip now) queueFiles
  liftIO $ print cips
  forM_ cips $ \cip-> do
    eUnpined <- unpinByCipRequest cip
    liftIO $ print eUnpined
    liftIO $ putStrLn "Burned token put into the queue for unpinning"

selectRottenCip :: POSIXTime -> FilePath -> IO (Maybe Text)
selectRottenCip now tokenPath = do
  eRotten <- eitherDecodeFileStrict' tokenPath
  case eRotten of
    Left err -> do
      putStrLn err
      pure Nothing
    Right rotten
      | rtRemoveTime rotten <= now -> pure $ Just $ rtCip rotten
      | otherwise -> pure Nothing


rottenTokenHandler :: IpfsEnv -> IO ()
rottenTokenHandler env = flip runReaderT env $ forever $ do
  removeRottenTokens (envScheduleDirectory env)
  -- Sleep for 12 hours (in microseconds)
  liftIO $ threadDelay $ 12 * 60 * 60 * 1000000
