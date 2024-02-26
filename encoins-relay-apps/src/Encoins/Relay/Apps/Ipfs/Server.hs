{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}



module Encoins.Relay.Apps.Ipfs.Server where

import           Encoins.Common.Constant        (column, space)
import           Encoins.Common.Log             (logDebugS, logError, logErrorS,
                                                 logInfo, logInfoS, logWarn)
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
import           Control.Exception.Safe         (SomeException, catchAny, throw,
                                                 tryAny)
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
import           Data.Maybe                     (catMaybes)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX          (POSIXTime, getPOSIXTime,
                                                 posixDayLength,
                                                 utcTimeToPOSIXSeconds)
import           Development.GitRev             (gitCommitDate, gitHash)
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
  say $ showAppVersion "IPFS server" $ appVersion version $(gitHash) $(gitCommitDate)
  withIpfsEnv $ \env -> do
    checkPinataToken env
    -- withAsync (rottenTokenHandler env) $ \_ -> do
    withRecovery "server" $ run (envPort env) $ app env

type ServerIpfsApi =
          "ping"
              :> Get '[JSON] NoContent
      :<|> "minted"
              :> ReqBody '[JSON] (AesKeyHash, [CloudRequest])
              :> Post '[JSON] (Map AssetName CloudResponse )

    --  :<|> "restore"
    --           :> Capture "client_id" Text
    --           :> Get '[JSON] [RestoreResponse]

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = ping
           :<|> minted
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

ping :: IpfsMonad NoContent
ping = do
  logInfo "Ping request"
  pure NoContent

minted :: (AesKeyHash, [CloudRequest]) -> IpfsMonad (Map AssetName CloudResponse)
minted (clientId, reqs) = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  mapM_ (mintedToken clientId responseTVar) reqs
  liftIO $ readTVarIO responseTVar

-- Pin minted tokens on IPFS and return their statuses
mintedToken :: AesKeyHash
  -> TVar (Map AssetName CloudResponse)
  -> CloudRequest
  -> IpfsMonad ()
mintedToken clientId tVar req = do
  isFormat <- asks envFormatMessage
  logInfo ""
  logInfo "Minted token received"
  logInfoS isFormat req
  let assetName = reqAssetName req
  coinStatus <- checkCoinStatus assetName
  logInfo $ "Coin status" <> column
  logInfoS isFormat coinStatus
  case coinStatus of
    CoinError err -> do
      logErrorS isFormat err
      modifyCacheResponse tVar assetName $ MkCloudResponse Nothing (Just coinStatus)
    Discarded -> do
      logInfo "Token can't be rollback anymore"
      modifyCacheResponse tVar assetName $ MkCloudResponse Nothing (Just Discarded)
    cStatus -> do
      ipfsStatus <- checkIpfsStatus assetName clientId
      logInfo $ "IPFS status" <> column <> space <> toText ipfsStatus
      case ipfsStatus of
        IpfsError iErr -> do
          logErrorS isFormat iErr
          modifyCacheResponse tVar assetName $ MkCloudResponse (Just ipfsStatus) (Just cStatus)
        Pinned -> do
          modifyCacheResponse tVar assetName
            (MkCloudResponse (Just Pinned) (Just cStatus))
        Unpinned -> do
          res <- pinJsonRequest $ mkTokentoIpfs clientId req
          case res of
            Left err -> do
              logErrorS isFormat err
              modifyCacheResponse tVar assetName $ MkCloudResponse
                (Just $ IpfsError $ toText err) (Just cStatus)
            Right r -> do
              logInfo "Pin response:"
              logInfoS isFormat r
              modifyCacheResponse tVar assetName
                (MkCloudResponse (Just Pinned) (Just cStatus))

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
    Right assets -> case getAsset assets of
      Left err -> do
        logErrorS isFormat err
        pure $ CoinError $ toText err
      Right asset
        | ambrAmount asset >= 0 -> pure Minted -- TODO: check 0 case
        | otherwise -> do
            now <- liftIO getCurrentTime
            if add12Hours (ambrTimestamp asset) > now
              then pure Burned
              else pure Discarded

add12Hours :: UTCTime -> UTCTime
add12Hours = addUTCTime (12 * 60 * 60)

checkIpfsStatus :: AssetName -> AesKeyHash -> IpfsMonad IpfsStatus
checkIpfsStatus assetName clientId  = do
  let assetNameT = getAssetName assetName
  logInfo $ "Check IPFS status for assetName" <> column <> space <> assetNameT
  files <- fetchByStatusNameKeyvalueRequest "pinned" assetName clientId
  case files of
    Left err -> do
      logError $ "fetchByStaIpfsErrortusNameRequest error" <> column <> space <> toText err
      pure $ IpfsError $ toText err
    Right fs -> do
      let names = catMaybes . map (mrName . metadata) . rows $ fs
      let validNames = filter (== assetName) names
      case length validNames of
        1 -> pure Pinned
        0 -> pure Unpinned
        n -> do
          logWarn $ "Token"
            <> space <> assetNameT
            <> space <> "pinned"
            <> space <> toText n
            <> space <> "times"
          pure Pinned

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

withRecovery :: Text -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      say $ "Exception caught in" <> space <> nameOfAction <> column <> space <> toText e
      liftIO $ threadDelay $ 5 * 1000000
      withRecovery nameOfAction action

checkPinataToken :: IpfsEnv -> IO ()
checkPinataToken env = runIpfsMonad env $ do
  isFormat <- asks envFormatMessage
  logInfo ""
  logInfo "Checking pinata token..."
  res <- testAuthenticationRequest
  case res of
    Left err -> do
      logDebugS isFormat err
      logError "Pinata token is invalid"
      throw InvalidPinataToken
    Right r -> do
      logDebugS isFormat r
      logInfo "Pinata token is valid"




-- Following functions not used for now.
-- It can be useful for restoring and cleaning cache

restore :: AesKeyHash -> IpfsMonad [RestoreResponse]
restore clientId = do
  eFiles <- fetchByStatusKeyvalueRequest "pinned" clientId
  case eFiles of
    Left err -> do
      sayShow err
      pure []
    Right (rows -> files) -> do
      (errors, rRes) <- fmap partitionEithers $ forM files $ \file -> do
        let mAssetName = mrName $ metadata file
        case mAssetName of
          Nothing -> pure $ Left $ InvalidStatus $ CoinError "Absent AssetName in metadata"
          Just assetName -> do
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
  logInfo "Burned token received"
  networkId <- asks envNetworkId
  currentSymbol <- asks envIpfsCurrencySymbol
  let assetName = reqAssetName t
  res <- getAssetMintsAndBurns networkId currentSymbol
    $ fromString
    $ T.unpack
    $ getAssetName assetName
  case getAsset res of
    Left err -> do
      logError err
      pure err
    Right asset
      | ambrAmount asset < 0 -> do
          env <- ask
          let burnDir = envScheduleDirectory env
          cips <- getBurnedCips assetName
          isFormat <- asks envFormatMessage
          logInfoS isFormat cips
          putInQueue burnDir assetName (ambrTimestamp asset) cips
          pure "Burned token put in queue for unpinning from ipfs"
      | otherwise -> do
          logInfo "Token found on the blockchain. Thus it is should not be unpinned"
          pure "Not unpinned"

putInQueue :: FilePath -> AssetName -> UTCTime -> [Cip] -> IpfsMonad ()
putInQueue scheduleDir assetName burnedTime cips = do
  let burnedTimePosix = utcTimeToPOSIXSeconds burnedTime
  let halfDay = posixDayLength / 2
  let removeTime = burnedTimePosix + halfDay
  liftIO $ createDirectoryIfMissing False scheduleDir
  mapM_ (\cip@(MkCip c) -> liftIO $ encodeFile (scheduleDir </> T.unpack c <.> "json")
    $ MkRottenToken assetName removeTime cip) cips

getBurnedCips :: AssetName -> IpfsMonad [Cip]
getBurnedCips assetName = do
  eFiles <- fetchByStatusNameRequest "pinned" assetName
  case eFiles of
    Left err -> do
      logError $ "fetchByStatusNameRequest error" <> column <> space <> toText err
      pure []
    Right ((map ipfsPinHash . rows) -> cips) -> pure cips


removeRottenTokens :: IpfsMonad ()
removeRottenTokens  = do
  isFormat <- asks envFormatMessage
  burnedDirectory <- asks envScheduleDirectory
  queueFiles <- liftIO $ listFiles burnedDirectory
  now <- liftIO $ getPOSIXTime
  cips <- liftIO $ mapMaybeM (selectRottenCip now) queueFiles
  logInfoS isFormat cips
  forM_ cips $ \(cip, path)-> do
    eUnpined <- unpinByCipRequest cip
    logInfoS isFormat eUnpined
    case eUnpined of
      Left err ->
        logErrorS isFormat err
      Right _ -> do
        logInfo "Burned token unpinned"
        eRemoved <- liftIO $ tryAny $ removeFile path
        case eRemoved of
          Left err -> do
            logError "Fail to remove file"
            logErrorS isFormat err
          Right _  -> logInfo "Burned filed removed from queue"
  pure ()

selectRottenCip :: POSIXTime -> FilePath -> IO (Maybe (Cip, FilePath))
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
