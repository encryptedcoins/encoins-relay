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
import           Control.Concurrent.STM
import           Control.Exception.Safe         (Exception, SomeException,
                                                 catchAny, throwM, toException,
                                                 tryAny)
import           Control.Monad                  (forM, forever, void)
import           Control.Monad.Extra            (forM_, mapMaybeM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (MonadReader (ask),
                                                 ReaderT (..), asks)
import           Data.Aeson                     (eitherDecodeFileStrict',
                                                 encode, encodeFile)
import           Data.ByteString.Lazy           (toStrict)
import           Data.Either                    (partitionEithers)
import           Data.List                      (sortOn)
import           Data.List.Extra                (unsnoc)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Data.Time                      (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime,
                                                 posixDayLength,
                                                 utcTimeToPOSIXSeconds)
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Say
import           Servant
import           System.Directory               (createDirectoryIfMissing,
                                                 removeFile)
import           System.Directory.Extra         (listFiles)
import           System.FilePath.Posix          (takeFileName, (<.>), (</>))

-- import           Data.ByteString                (ByteString)
-- import qualified Data.ByteString                as BS
-- import qualified Data.ByteString.Char8          as BSC8
-- import           Data.Time.Format
-- import           Servant.Client
-- import           Control.Concurrent.Async       (withAsync)

type ServerIpfsApi =
          "cache"
              :> ReqBody '[JSON] (Text, [CloudRequest])
              :> Post '[JSON] (Map Text CloudResponse )
     :<|> "restore"
              :> Capture "client_id" Text
              :> Get '[JSON] [RestoreResponse]

    -- :<|> "minted" :> ReqBody '[JSON] CloudRequest
    --               :> Post '[JSON] CloudResponse
    -- :<|> "burned" :> ReqBody '[JSON] TokenToCloud :> Post '[JSON] Text

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = cache
           :<|> restore

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

cache :: (Text, [CloudRequest]) -> IpfsMonad (Map Text CloudResponse)
cache (clientId, reqs) = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  mapM_ (cacheToken clientId responseTVar) reqs
  liftIO $ readTVarIO responseTVar

cacheToken :: Text
  -> TVar (Map Text CloudResponse)
  -> CloudRequest
  -> IpfsMonad ()
cacheToken clientId tVar req = do
  say "Minted token received"
  sayShow req
  let assetName = reqAssetName req
  eAssets <- liftIO $ tryAny $ getAssetMintsAndBurns
    (Testnet $ NetworkMagic 1)
    "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
    (fromString $ T.unpack assetName)
  sayShow eAssets
  case eAssets of
    Left err -> do
      sayShow err
      modifyCacheResponse tVar assetName
        $ MkCloudResponse Nothing
        $ Just
        $ CoinError
        $ T.pack
        $ show err
    Right assets -> case ambrAmount <$> getAsset assets of
      Left err -> do
        say err
        modifyCacheResponse tVar assetName
          $ MkCloudResponse Nothing
          $ Just
          $ CoinError
          $ T.pack
          $ show err
      Right x
        | x > 0 -> do
            res <- pinJsonRequest $ mkTokentoIpfs clientId req
            case res of
              Left err -> do
                sayShow err
                modifyCacheResponse tVar assetName $ MkCloudResponse
                  (Just $ FileError $ T.pack $ show err) Nothing
              Right r -> do
                sayShow r
                modifyCacheResponse tVar assetName
                  (MkCloudResponse (Just Pinned) (Just Minted))
        | otherwise -> do
            say "Token found in blockchain and it burned"
            modifyCacheResponse tVar assetName
              (MkCloudResponse Nothing (Just Burned))

modifyCacheResponse :: TVar (Map Text CloudResponse)
  -> Text
  -> CloudResponse
  -> IpfsMonad ()
modifyCacheResponse tVar assetName resp
  = liftIO
  $ atomically
  $ modifyTVar' tVar (Map.insert assetName resp)

-- TODO: add check token is still minted
restore :: Text -> IpfsMonad [RestoreResponse]
restore clientId = do
  eFiles <- fetchByStatusKeyvalueRequest "pinned" clientId
  case eFiles of
    Left err -> do
      sayShow err
      pure []
    Right (rows -> files) -> do
      (errors, rRes) <- fmap partitionEithers $ forM files $ \file -> do
        eTokenKey <- fetchByCipRequest (ipfsPinHash file)
        let name = fromMaybe "absentAssetName" $ mrName $ metadata file
        pure $ MkRestoreResponse name <$> eTokenKey
      case errors of
        [] -> pure rRes
        _ -> do
          mapM_ sayShow errors
          pure []


-- minted :: CloudRequest -> IpfsMonad CloudResponse
-- minted t = do
--   say "Minted token received"
--   sayShow t
--   let assetName = reqAssetName t
--   eAssets <- liftIO $ tryAny $ getAssetMintsAndBurns
--     (Testnet $ NetworkMagic 1)
--     "fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
--     (fromString $ T.unpack assetName)
--   sayShow eAssets
--   case eAssets of
--     Left err -> do
--       sayShow err
--       pure $ MkCloudResponse assetName Nothing $ Just $ CoinError $ T.pack $ show err
--     Right assets -> case ambrAmount <$> getAsset assets of
--       Left err -> do
--         say err
--         pure $ MkCloudResponse assetName Nothing $ Just $ CoinError $ T.pack $ show err
--       Right x
--         | x > 0 -> do
--             res <- pinJsonRequest $ mkTokentoIpfs t
--             case res of
--               Left err -> do
--                 sayShow err
--                 pure $ MkCloudResponse assetName (Just $ FileError $ T.pack $ show err) Nothing
--               Right r -> do
--                 sayShow r
--                 pure $ MkCloudResponse assetName (Just Pinned) (Just Minted)
--         | otherwise -> do
--             say "Token found in blockchain and it burned"
--             pure $ MkCloudResponse assetName Nothing (Just Burned)

getAsset :: [AssetMintsAndBurnsResponse] -> Either Text AssetMintsAndBurnsData
getAsset res = do
  resUniq <- if length res == 1
        then Right $ head res
        else Left "getAssetMintsAndBurns returned more than one asset."
  -- Sorting by slot is just in case
  let asset = sortOn ambrSlot $ ambrData resUniq
  let assetSorted = snd <$> (unsnoc asset)
  case assetSorted of
    Nothing -> Left "ambrData is empty"
    Just a  -> Right a

burned :: CloudRequest -> IpfsMonad Text
burned t = do
  say "Burned token received"
  -- sayShow t
  let assetName = reqAssetName t
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
  eFiles <- fetchByStatusNameRequest "pinned" assetName
  case eFiles of
    Left err -> do
      liftIO
        $ putStrLn
        $ "fetchByStatusNameRequest error: " <> show err
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
