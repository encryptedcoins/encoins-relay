{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}


module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Common.Constant        (column, space)
import           Encoins.Common.Log             (logErrorS, logInfo, logInfoS)
import           Encoins.Common.Transform       (toText)
import           Encoins.Common.Version         (appVersion, showAppVersion)
import           Encoins.Relay.Apps.Ipfs.Client
import           Encoins.Relay.Apps.Ipfs.Config
import           Encoins.Relay.Apps.Ipfs.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro  (AssetMintsAndBurnsData (..),
                                                 AssetMintsAndBurnsResponse (..))

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception.Safe         (Exception, SomeException,
                                                 catchAny, tryAny)
import           Control.Monad                  (forM, forever)
import           Control.Monad.Extra            (forM_, mapMaybeM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (MonadReader (ask),
                                                 ReaderT (..), asks)
import           Data.Aeson                     (eitherDecodeFileStrict',
                                                 encodeFile)
import           Data.Either                    (partitionEithers)
import           Data.Either.Extra              (mapLeft)
import           Data.List                      (sortOn)
import           Data.List.Extra                (unsnoc)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime,
                                                 posixDayLength,
                                                 utcTimeToPOSIXSeconds)
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Paths_encoins_relay_apps       (version)
import           Say
import           Servant
import           System.Directory               (createDirectoryIfMissing,
                                                 removeFile)
import           System.Directory.Extra         (listFiles)
import           System.FilePath.Posix          ((<.>), (</>))

ipfsServer :: IO ()
ipfsServer = do
  say $ showAppVersion "IPFS server" $ appVersion version
  withIpfsEnv $ \env -> do
    -- withAsync (rottenTokenHandler env) $ \_ -> do
    withRecovery "server" $ run (envPort env) $ app env

type ServerIpfsApi =
          "cache"
              :> ReqBody '[JSON] (AesKeyHash, [CloudRequest])
              :> Post '[JSON] (Map AssetName CloudResponse )
    --  :<|> "restore"
    --           :> Capture "client_id" Text
    --           :> Get '[JSON] [RestoreResponse]

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = cache
          --  :<|> restore

handlerServer :: IpfsEnv -> ServerT ServerIpfsApi Handler
handlerServer env = hoistServer serverIpfsApiProxy (liftIO . runIpfsMonad env) serverIpfsApi

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }

app :: IpfsEnv -> Application
app = corsWithContentType . serve serverIpfsApiProxy . handlerServer

-- TODO: get rid of liftIO

cache :: (AesKeyHash, [CloudRequest]) -> IpfsMonad (Map AssetName CloudResponse)
cache (clientId, reqs) = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  mapM_ (cacheToken clientId responseTVar) reqs
  liftIO $ readTVarIO responseTVar

cacheToken :: AesKeyHash
  -> TVar (Map AssetName CloudResponse)
  -> CloudRequest
  -> IpfsMonad ()
cacheToken clientId tVar req = do
  isFormat <- asks envFormatMessage
  logInfo ""
  logInfo "Minted token received"
  logInfoS isFormat req
  let assetName = reqAssetName req
  coinStatus <- checkCoinStatus assetName
  logInfo $ "Coin status" <> column <> space <> toText coinStatus
  case coinStatus of
    CoinError _ -> do
      modifyCacheResponse tVar assetName $ MkCloudResponse Nothing (Just coinStatus)
    Burned -> do
      logInfo "Token found on blockchain and it was burned"
      modifyCacheResponse tVar assetName $ MkCloudResponse Nothing (Just Burned)
    Minted -> do
      res <- pinJsonRequest $ mkTokentoIpfs clientId req
      case res of
        Left err -> do
          logErrorS isFormat err
          modifyCacheResponse tVar assetName $ MkCloudResponse
            (Just $ FileError $ toText err) Nothing
        Right r -> do
          logInfo "Pin response:"
          logInfoS isFormat r
          modifyCacheResponse tVar assetName
            (MkCloudResponse (Just Pinned) (Just Minted))

modifyCacheResponse :: TVar (Map AssetName CloudResponse)
  -> AssetName
  -> CloudResponse
  -> IpfsMonad ()
modifyCacheResponse tVar assetName resp
  = liftIO
  $ atomically
  $ modifyTVar' tVar (Map.insert assetName resp)

checkCoinStatus :: AssetName -> IpfsMonad CoinStatus
checkCoinStatus assetName = do
  isFormat <- asks envFormatMessage
  networkId <- asks envNetworkId
  currentSymbol <- asks envIpfsCurrencySymbol
  logInfo $ "Check coin status for assetName" <> column <> space <> getAssetName assetName
  eAssets <- tryAny $ getAssetMintsAndBurns networkId currentSymbol
    $ fromString $ T.unpack $ getAssetName assetName
  logInfo $ "Maestro response:"
  logInfoS isFormat eAssets
  case eAssets of
    Left err -> do
      logErrorS isFormat err
      pure $ CoinError $ toText err
    Right assets -> case ambrAmount <$> getAsset assets of
      Left err -> do
        logErrorS isFormat err
        pure $ CoinError $ toText err
      Right x
        | x > 0 -> pure Minted
        | otherwise -> pure Burned

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

-- Following functions not used for now.
-- It can be useful for restore and cleaning cache

restore :: AesKeyHash -> IpfsMonad [RestoreResponse]
restore clientId = do
  eFiles <- fetchByStatusKeyvalueRequest "pinned" clientId
  case eFiles of
    Left err -> do
      sayShow err
      pure []
    Right (rows -> files) -> do
      (errors, rRes) <- fmap partitionEithers $ forM files $ \file -> do
        let assetName = MkAssetName $ fromMaybe "invalidAssetName" $ mrName $ metadata file
        coinStatus <- checkCoinStatus assetName
        case coinStatus of
          Minted -> do
            eSecretIpfs <- fetchByCipRequest (ipfsPinHash file)
            let eEncSecret = MkEncryptedSecret . getEncryptedToken <$> eSecretIpfs
            pure $ mapLeft Client $ MkRestoreResponse assetName <$> eEncSecret
          _ -> pure $ Left $ InvalidStatus coinStatus
      case errors of
        [] -> pure rRes
        _ -> do
          mapM_ sayShow errors
          pure []

burned :: CloudRequest -> IpfsMonad Text
burned t = do
  say "Burned token received"
  networkId <- asks envNetworkId
  currentSymbol <- asks envIpfsCurrencySymbol
  let assetName = reqAssetName t
  res <- getAssetMintsAndBurns networkId currentSymbol
    $ fromString
    $ T.unpack
    $ getAssetName assetName
  case getAsset res of
    Left err -> do
      say err
      pure err
    Right asset
      | ambrAmount asset < 0 -> do
          env <- ask
          let burnDir = envScheduleDirectory env
          cips <- getBurnedCips assetName
          sayShow cips
          putInQueue burnDir assetName (ambrTimestamp asset) cips
          pure "Burned token put in queue for unpinning from ipfs"
      | otherwise -> do
          say "Token found on the blockchain. Thus it is should not be unpinned"
          pure "Not unpinned"

putInQueue :: FilePath -> AssetName -> UTCTime -> [Text] -> IpfsMonad ()
putInQueue scheduleDir assetName burnedTime cips = do
  let burnedTimePosix = utcTimeToPOSIXSeconds burnedTime
  let halfDay = posixDayLength / 2
  let removeTime = burnedTimePosix + halfDay
  liftIO $ createDirectoryIfMissing False scheduleDir
  mapM_ (\cip -> liftIO $ encodeFile (scheduleDir </> T.unpack cip <.> "json")
    $ MkRottenToken assetName removeTime cip) cips

getBurnedCips :: AssetName -> IpfsMonad [Text]
getBurnedCips assetName = do
  eFiles <- fetchByStatusNameRequest "pinned" assetName
  case eFiles of
    Left err -> do
      say $ "fetchByStatusNameRequest error" <> column <> space <> toText err
      pure []
    Right ((map ipfsPinHash . rows) -> cips) -> pure cips


removeRottenTokens :: IpfsMonad ()
removeRottenTokens  = do
  burnedDirectory <- asks envScheduleDirectory
  queueFiles <- liftIO $ listFiles burnedDirectory
  now <- liftIO $ getPOSIXTime
  cips <- liftIO $ mapMaybeM (selectRottenCip now) queueFiles
  sayShow cips
  -- res <- throwM ThisException
  -- sayShow @_ @MyException res
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

withRecovery :: Text -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      say $ "Exception caught in" <> space <> nameOfAction <> column <> space <> toText e
      liftIO $ threadDelay $ 5 * 1000000
      withRecovery nameOfAction action


data MyException = ThisException | ThatException
    deriving Show

instance Exception MyException
