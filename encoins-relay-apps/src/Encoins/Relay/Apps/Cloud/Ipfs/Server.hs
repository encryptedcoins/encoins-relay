{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}



module Encoins.Relay.Apps.Cloud.Ipfs.Server where

import           Encoins.Common.Constant              (column, space)
import           Encoins.Common.Log                   (logDebug, logDebugS,
                                                       logError, logErrorS,
                                                       logInfo, logInfoS,
                                                       logWarn)
import           Encoins.Common.Transform             (toText)
import           Encoins.Common.Version               (appVersion,
                                                       showAppVersion)
import           Encoins.Relay.Apps.Cloud.Ipfs.Config
import           Encoins.Relay.Apps.Cloud.Ipfs.Request
import           Encoins.Relay.Apps.Cloud.Ipfs.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro        (AssetMintsAndBurnsData (..),
                                                       AssetMintsAndBurnsResponse (..))

import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception.Safe               (SomeException, catchAny,
                                                       throw, tryAny)
import           Control.Monad                        (forever)
import           Control.Monad.Extra                  (forM_, mapMaybeM)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Reader                 (ReaderT (..), asks)
import           Data.Aeson                           (eitherDecodeFileStrict',
                                                       encodeFile)
import qualified Data.ByteString.Char8                as B
import qualified Data.List.NonEmpty                   as NE
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.String                          (IsString (fromString))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import           Data.Time                            (UTCTime, addUTCTime,
                                                       getCurrentTime)
import           Data.Time.Clock.POSIX                (POSIXTime, getPOSIXTime,
                                                       posixDayLength,
                                                       utcTimeToPOSIXSeconds)
import           Development.GitRev                   (gitCommitDate, gitHash)
import qualified Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..),
                                                       cors,
                                                       simpleCorsResourcePolicy)
import           Paths_encoins_relay_apps             (version)
import           Servant
import           System.Directory                     (createDirectoryIfMissing,
                                                       removeFile)
import           System.Directory.Extra               (listFiles)
import           System.FilePath.Posix                ((<.>), (</>))
import           Text.Pretty.Simple                   (pPrint, pPrintString)

ipfsServer :: IO ()
ipfsServer = do
  B.putStrLn
    $ TE.encodeUtf8
    $  showAppVersion "IPFS server" $ appVersion version $(gitHash) $(gitCommitDate)
  withIpfsEnv $ \env -> do
    checkPinataToken env
    -- withAsync (rottenTokenHandler env) $ \_ -> do
    withRecovery "server" $ run (envPort env) $ app env

type ServerIpfsApi =
           "ping"
              :> Get '[JSON] NoContent
      :<|> "pin"
              :> ReqBody '[JSON] (AesKeyHash, [PinRequest])
              :> Post '[JSON] (Map AssetName StatusResponse)

serverIpfsApiProxy :: Proxy ServerIpfsApi
serverIpfsApiProxy = Proxy

serverIpfsApi :: ServerT ServerIpfsApi IpfsMonad
serverIpfsApi = ping
           :<|> pin
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

pin :: (AesKeyHash, [PinRequest]) -> IpfsMonad (Map AssetName StatusResponse)
pin (clientId, reqs) = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  mapM_ (pinToken clientId responseTVar) reqs
  liftIO $ readTVarIO responseTVar

-- Pin minted tokens on IPFS and return their statuses
pinToken :: AesKeyHash
  -> TVar (Map AssetName StatusResponse)
  -> PinRequest
  -> IpfsMonad ()
pinToken clientId tVar req = do
  isFormat <- asks envFormatMessage
  logInfo ""
  logInfo "Unpinned token received"
  logDebugS isFormat req
  let assetName = ppAssetName req
  coinStatus <- checkCoinStatus assetName
  logInfo $ "Coin status" <> column <> space <> toText coinStatus
  ipfsStatus <- case coinStatus of
    CoinError err -> do
      logErrorS isFormat err
      pure $ MkStatusResponse IpfsError
    CoinDiscarded m -> do
      logInfo "Token has been discarded and can't be rollback anymore"
      logInfo $ "The reason of discarding" <> column <> space <> m
      pure $ MkStatusResponse Discarded
    _ -> do
      ipfsResp <- checkIpfsStatus assetName clientId
      logInfo $ "Old IPFS status" <> column <> space <> toText ipfsResp
      case ipfsResp of
        IpfsFail ipfsErr -> do
          logErrorS isFormat ipfsErr
          pure $ MkStatusResponse IpfsError
        IpfsPinned -> do
          pure $ MkStatusResponse Pinned
        IpfsUnpinned -> do
          res <- pinJsonRequest $ mkTokentoIpfs clientId req
          case res of
            Left err -> do
              logErrorS isFormat err
              pure $ MkStatusResponse IpfsError
            Right r -> do
              logDebug "Pin response:"
              logDebugS isFormat r
              pure $ MkStatusResponse Pinned
  logInfo $ "New IPFS status" <> column <> space <>
    toText (spStatusResponse ipfsStatus)
  modifyCacheResponse tVar assetName ipfsStatus


modifyCacheResponse :: TVar (Map AssetName StatusResponse)
  -> AssetName
  -> StatusResponse
  -> IpfsMonad ()
modifyCacheResponse tVar assetName resp
  = liftIO
  $ atomically
  $ modifyTVar' tVar (Map.insert assetName resp)

checkCoinStatus :: AssetName -> IpfsMonad CoinStatus
checkCoinStatus assetName = do
  isFormat <- asks envFormatMessage
  currentSymbol <- asks envIpfsCurrencySymbol
  logInfo $ "Check coin status for assetName" <> column <> space <> getAssetName assetName
  eAssets <- tryAny $ getAssetMintsAndBurns currentSymbol
    $ fromString $ T.unpack $ getAssetName assetName
  logDebug "Maestro response:"
  logDebugS isFormat eAssets
  case eAssets of
    Left err -> do
      logErrorS isFormat err
      pure $ CoinError $ toText err
    Right resAssets -> case length resAssets of
      1 -> case NE.nonEmpty $ ambrData $ head resAssets of
        Nothing -> do
          logWarn "ambrData is empty"
          pure $ CoinDiscarded "ambrData is empty"
        Just (NE.last . NE.sortWith ambrSlot -> asset)
          | ambrAmount asset == 1 -> pure CoinMinted
          | ambrAmount asset == (-1) -> do
              now <- liftIO getCurrentTime
              let delta = discardTime (ambrTimestamp asset)
              if delta > now
                then do
                  logInfo $ "Discarding time" <> space <> toText delta
                  pure CoinBurned
                else pure $ CoinDiscarded "Rollback is impossible"
          | otherwise -> do
              let mes = "Asset amount is invalid: " <> toText (ambrAmount asset)
              logError mes
              pure $ CoinDiscarded mes
      _ -> do
        logError "getAssetMintsAndBurns returned more than one asset"
        pure $ CoinDiscarded "more than one asset"

checkIpfsStatus :: AssetName -> AesKeyHash -> IpfsMonad IpfsResponse
checkIpfsStatus assetName clientId  = do
  let assetNameT = getAssetName assetName
  let clientIdT = getAesKeyHash clientId
  logInfo $ "Check IPFS status"
  logInfo $ "for assetName" <> column <> space <> assetNameT
  logInfo $ "and clientId" <> column <> space <> clientIdT
  files <- fetchByStatusNameKeyvalueRequest "pinned" assetName clientId
  case files of
    Left err -> do
      logError $ "fetchByStatusNameKeyvalueRequest error" <> column <> space <> toText err
      pure $ IpfsFail $ toText err
    Right fs -> case length $ rows fs of
      1 -> pure IpfsPinned
      0 -> pure IpfsUnpinned
      n -> do
        logWarn $ "Token"
          <> space <> assetNameT
          <> space <> "pinned"
          <> space <> toText n
          <> space <> "times"
        pure IpfsPinned

withRecovery :: Text -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      pPrint $ "Exception caught in" <> space <> nameOfAction <> column <> space <> toText e
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

-- Discarding burned token in 12 hours
discardTime :: UTCTime -> UTCTime
discardTime = addUTCTime (12 * 60 * 60)


-- Following functions not used for now.
-- It can be useful for restoring and cleaning cache

-- restore :: AesKeyHash -> IpfsMonad [RestoreResponse]
-- restore clientId = do
--   eFiles <- fetchByStatusKeyvalueRequest "pinned" clientId
--   case eFiles of
--     Left err -> do
--       pPrint err
--       pure []
--     Right (rows -> files) -> do
--       (errors, rRes) <- fmap partitionEithers $ forM files $ \file -> do
--         let mAssetName = mrName $ metadata file
--         case mAssetName of
--           Nothing -> pure $ Left $ InvalidStatus $ CoinError "Absent AssetName in metadata"
--           Just assetName -> do
--             coinStatus <- checkCoinStatus assetName
--             case coinStatus of
--               CoinMinted -> do
--                 eSecretIpfs <- fetchByCipRequest (ipfsPinHash file)
--                 let eEncSecret = MkEncryptedSecret . getEncryptedToken <$> eSecretIpfs
--                 pure $ mapLeft Client $ MkRestoreResponse assetName <$> eEncSecret
--               _ -> pure $ Left $ InvalidStatus coinStatus
--       case errors of
--         [] -> pure rRes
--         _ -> do
--           mapM_ pPrint errors
--           pure []

-- burned :: PinRequest -> IpfsMonad Text
-- burned t = do
--   logInfo "CoinBurned token received"
--   networkId <- asks envNetworkId
--   currentSymbol <- asks envIpfsCurrencySymbol
--   let assetName = ppAssetName t
--   res <- getAssetMintsAndBurns networkId currentSymbol
--     $ fromString
--     $ T.unpack
--     $ getAssetName assetName
--   case selectLastData res of
--     Left err -> do
--       logError err
--       pure err
--     Right asset
--       | ambrAmount asset < 0 -> do
--           env <- ask
--           let burnDir = envScheduleDirectory env
--           cips <- getBurnedCips assetName
--           isFormat <- asks envFormatMessage
--           logInfoS isFormat cips
--           putInQueue burnDir assetName (ambrTimestamp asset) cips
--           pure "CoinBurned token put in queue for unpinning from ipfs"
--       | otherwise -> do
--           logInfo "Token found on the blockchain. Thus it is should not be unpinned"
--           pure "Not unpinned"

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
        logInfo "CoinBurned token unpinned"
        eRemoved <- liftIO $ tryAny $ removeFile path
        case eRemoved of
          Left err -> do
            logError "Fail to remove file"
            logErrorS isFormat err
          Right _  -> logInfo "CoinBurned filed removed from queue"
  pure ()

selectRottenCip :: POSIXTime -> FilePath -> IO (Maybe (Cip, FilePath))
selectRottenCip now tokenPath = do
  eRotten <- liftIO $ eitherDecodeFileStrict' tokenPath
  case eRotten of
    Left err -> do
      pPrintString err
      pure Nothing
    Right rotten
      | rtRemoveTime rotten <= now -> pure $ Just $ (rtCip rotten, tokenPath)
      | otherwise -> pure Nothing


rottenTokenHandler :: IpfsEnv -> IO ()
-- rottenTokenHandler env = withRecovery "removeRottenTokens" $ forever $ do
rottenTokenHandler env = forever $ do
      pPrintString "run removeRottenTokens"
      pPrint =<< getCurrentTime
      pPrint =<< getPOSIXTime
      runReaderT (unIpfsMonad removeRottenTokens) env
      -- Sleep for 1 hour (in microseconds)
      -- liftIO $ threadDelay $ 60 * 60 * 1000000
      threadDelay $ 60 * 1000000
