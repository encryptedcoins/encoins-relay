{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}



module Encoins.Relay.Apps.Cloud.Server where

import           Encoins.Common.Constant                   (column, space)
import           Encoins.Common.Log                        (logDebug, logDebugS,
                                                            logError, logErrorS,
                                                            logInfo, logInfoS)
import           Encoins.Common.Transform                  (toText)
import           Encoins.Common.Version                    (appVersion,
                                                            showAppVersion)
import           Encoins.Relay.Apps.Cloud.Config
import           Encoins.Relay.Apps.Cloud.PostgreSQL.Query (deleteDiscardedTokensS,
                                                            getAllSavedTokensS,
                                                            insertDiscardedTokensS,
                                                            insertOnAbsentS,
                                                            selectAllDiscardedTokensS,
                                                            selectStaleDiscardedTokensS,
                                                            selectUniqSavedTokensS)
import           Encoins.Relay.Apps.Cloud.Types
import           Paths_encoins_relay_apps                  (version)
import           PlutusAppsExtra.IO.Maestro                (getAssetMintsAndBurns)
import           PlutusAppsExtra.Utils.Maestro             (AssetMintsAndBurnsData (..),
                                                            AssetMintsAndBurnsResponse (..))

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.Async                  (withAsync)
import           Control.Concurrent.STM
import           Control.Exception.Safe                    (SomeException,
                                                            catchAny, tryAny)
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (MonadIO (liftIO))
import           Control.Monad.Reader                      (asks)
import qualified Data.ByteString.Char8                     as B
import           Data.Char                                 (toLower)
import qualified Data.List.NonEmpty                        as NE
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import qualified Data.Set                                  as Set
import           Data.String                               (IsString (fromString))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import           Data.Time                                 (NominalDiffTime,
                                                            UTCTime, addUTCTime,
                                                            nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX                     (POSIXTime,
                                                            getPOSIXTime,
                                                            posixSecondsToUTCTime,
                                                            utcTimeToPOSIXSeconds)
import           Data.Vector                               (Vector)
import qualified Data.Vector                               as V
import           Development.GitRev                        (gitCommitDate,
                                                            gitHash)
import qualified Hasql.Pool                                as Pool
import qualified Network.Wai                               as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors               (CorsResourcePolicy (..),
                                                            cors,
                                                            simpleCorsResourcePolicy)
import           Servant


cloudServer :: IO ()
cloudServer = do
  B.putStrLn
    $ TE.encodeUtf8
    $ showAppVersion "Cloud server" $ appVersion version $(gitHash) $(gitCommitDate)
  withCloudEnv $ \env -> do
    withAsync (discard env) $ \_ -> do
      withRecovery "server" $ run (envPort env) $ app env

cleaner :: IO ()
cleaner = do
  B.putStrLn
    $ TE.encodeUtf8
    $ showAppVersion "Cleaner" $ appVersion version $(gitHash) $(gitCommitDate)
  withCloudEnv $ \env -> do
    clean env

type ServerSaveApi =
           "ping"
              :> Get '[JSON] NoContent
      :<|> "save"
              :> ReqBody '[JSON] [SaveRequest]
              :> Post '[JSON] (Map AssetName StatusResponse)
      :<|> "restore"
              :> Get '[JSON] (Vector (Text,Text))

serverSaveApiProxy :: Proxy ServerSaveApi
serverSaveApiProxy = Proxy

serverSaveApi :: ServerT ServerSaveApi CloudMonad
serverSaveApi = ping
           :<|> save
           :<|> restore

handlerServer :: CloudEnv -> ServerT ServerSaveApi Handler
handlerServer env = hoistServer serverSaveApiProxy (liftIO . runCloudMonad env) serverSaveApi

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }

app :: CloudEnv -> Application
app = corsWithContentType . serve serverSaveApiProxy . handlerServer

ping :: CloudMonad NoContent
ping = do
  logInfo "Ping request"
  pure NoContent

save :: [SaveRequest] -> CloudMonad (Map AssetName StatusResponse)
save reqs = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  pool <- asks envPool
  mapM_ (saveToken responseTVar pool) reqs
  liftIO $ readTVarIO responseTVar

-- Save unsaved token and return their status
saveToken :: TVar (Map AssetName StatusResponse)
  -> Pool.Pool
  -> SaveRequest
  -> CloudMonad ()
saveToken tVar pool req = do
  isFormat <- asks envFormatMessage
  let name = ppAssetName req
  let secret = ppSecretKey req
  logInfo ""
  logInfo $ "Received unsaved token" <> column <> space <> getAssetName name
  logDebugS isFormat req
  now <- liftIO getPOSIXTime
  eResult <- liftIO $ Pool.use pool $ insertOnAbsentS name secret now
  resp <- case eResult of
    Left err -> do
      logError "Insert error:"
      logErrorS isFormat err
      pure $ MkStatusResponse SaveError
    Right _ -> do
      logInfo "Token is saved"
      pure $ MkStatusResponse Saved
  modifyCacheResponse tVar name resp

modifyCacheResponse :: TVar (Map AssetName StatusResponse)
  -> AssetName
  -> StatusResponse
  -> CloudMonad ()
modifyCacheResponse tVar assetName resp
  = liftIO
  $ atomically
  $ modifyTVar' tVar (Map.insert assetName resp)

withRecovery :: Text -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      print $ "Exception caught in" <> space <> nameOfAction <> column <> space <> toText e
      liftIO $ threadDelay $ 5 * 1000000
      withRecovery nameOfAction action

restore :: CloudMonad (Vector (Text, Text))
restore = do
  isFormat <- asks envFormatMessage
  pool <- asks envPool
  logInfo ""
  logInfo "Restore query received"
  evTokens <- liftIO $ Pool.use pool getAllSavedTokensS
  case evTokens of
    Left err -> do
      logErrorS isFormat err
      pure V.empty
    Right tokens -> do
      logInfo $ "Restored"
        <> space <> toText (V.length tokens)
        <> space <> "tokens"
      pure tokens


discard :: CloudEnv -> IO ()
discard env = withRecovery "discard" $ runCloudMonad env $ do
  isFormat <- asks envFormatMessage
  pool <- asks envPool
  timeLag <- asks (truncate . nominalDiffTimeToSeconds . envDiscardPeriod)
  forever $ do
    logInfo ""
    logInfo "Run discard"
    evTokens <- liftIO $ Pool.use pool selectUniqSavedTokensS
    case evTokens of
      Left err -> do
        logErrorS isFormat err
      Right tokens -> do
        evOldDiscardedTokens <- liftIO $ Pool.use pool selectAllDiscardedTokensS
        case evOldDiscardedTokens of
          Left err -> do
            logErrorS isFormat err
          Right oldDiscardedTokens -> do
            time <- liftIO $ getPOSIXTime
            logDebug "Old discarded tokens"
            logDebugS isFormat oldDiscardedTokens
            logDebug "Saved tokens"
            logDebugS isFormat tokens
            let notDiscardedTokens = getNotDiscarded tokens oldDiscardedTokens
            logDebug "Not discarded tokens"
            logDebugS isFormat notDiscardedTokens
            discardedTokens <- V.mapMaybeM (detectDiscarded time) notDiscardedTokens
            logDebug ""
            logDebug "New discarded tokens"
            logDebugS isFormat discardedTokens
            case V.null discardedTokens of
              True -> logInfo "New discarded tokens was not found"
              False -> do
                res <- liftIO $ Pool.use pool $ insertDiscardedTokensS discardedTokens
                case res of
                  Left e -> do
                    logError "Inserting discarded tokens failed"
                    logErrorS isFormat e
                  Right _ -> do
                    logInfo ""
                    logInfo "Discarded tokens was inserted"

    -- Sleep for 12 hour (in microseconds)
    liftIO $ threadDelay $ timeLag * 1000000

detectDiscarded :: POSIXTime
  -> (AssetName, POSIXTime)
  -> CloudMonad (Maybe (AssetName, POSIXTime))
detectDiscarded now asset@(assetName, _) = do
  let assetNameT = getAssetName assetName
  isFormat <- asks envFormatMessage
  currentSymbol <- asks envCurrencySymbol
  logDebug $ "Check coin status for assetName" <> column <> space <> assetNameT
  eAssets <- tryAny $ getAssetMintsAndBurns currentSymbol
    $ fromString $ T.unpack assetNameT
  logDebug "Maestro response:"
  logDebugS isFormat eAssets
  case eAssets of
    Left err -> do
      logErrorS isFormat err
      logInfo $ assetNameT <> space <> "is discarded"
      pure Nothing
    Right assetResponses -> do
      timeLag <- asks envDiscardPeriod
      let mAsset = isDiscardedInList now timeLag asset assetResponses
      logDebug $ assetNameT <> space
        <> maybe "is alive" (const "is discarded") mAsset
      pure $ (,now) <$> mAsset

isDiscardedInList :: POSIXTime
  -> POSIXTime
  -> (AssetName, POSIXTime)
  -> [AssetMintsAndBurnsResponse]
  -> Maybe AssetName
isDiscardedInList now lag asset@(assetName, saveTime) assetResponses =
  case assetResponses of
    [] -> if saveTime + lag < now
      then Just assetName
      else Nothing
    _ -> if all (isDiscarded now lag asset) assetResponses
      then Just assetName
      else Nothing

isDiscarded :: POSIXTime
  -> POSIXTime
  -> (AssetName, POSIXTime)
  -> AssetMintsAndBurnsResponse
  -> Bool
isDiscarded now lag (_, saveTime) assetResponse =
  let mAssetStates = NE.nonEmpty $ ambrData assetResponse
  in case mAssetStates of
    Nothing -> saveTime + lag < now
    Just states ->
      let state = NE.last $ NE.sortWith ambrSlot states
      in case ambrAmount state of
          (-1) -> addTimeLag lag (ambrTimestamp state) < now
          _    -> False

clean :: CloudEnv -> IO ()
clean env = withRecovery "clean" $ runCloudMonad env $ do
  isFormat <- asks envFormatMessage
  pool <- asks envPool
  cleanDelay <- asks envCleanDelay
  now <- liftIO getPOSIXTime
  let staleTime = now - cleanDelay

  logInfo ""
  logInfo "Run clean"
  logInfo $ "Current time is" <> space
    <> toText now <> space
    <> "("
    <> toText (posixSecondsToUTCTime now)
    <> ")"
  logInfo $ "Stale time is" <> space
    <> toText staleTime <> space
    <> "("
    <> toText (posixSecondsToUTCTime staleTime)
    <> ")"

  -- Tokens that are older than Stale time are selected
  evTokens <- liftIO $ Pool.use pool $ selectStaleDiscardedTokensS staleTime
  case evTokens of
    Left err -> do
      logInfo "Selecting tokens for deleting was failed"
      logErrorS isFormat err
      pure ()
    Right vTokens -> do
      -- logDebugS isFormat vTokens
      case V.null vTokens of
        True -> logInfo "Discarded tokens was not found"
        False -> do
          logInfo "Selected tokens for deleting"
          logInfoS isFormat vTokens
          logInfo "Delete discarded tokens? y/N"
          reply <- liftIO $ getChar
          -- let reply = 'y'
          case toLower reply of
            'y' -> do
                logInfo $ "Deleting tokens that was discarded before" <> space
                  <> toText staleTime <> space
                  <> "("
                  <> toText (posixSecondsToUTCTime staleTime)
                  <> ")"
                eDeleteRes <- liftIO $ Pool.use pool $ deleteDiscardedTokensS vTokens
                case eDeleteRes of
                  Left err -> do
                    logInfo "Deleting tokens was failed"
                    logErrorS isFormat err
                    pure ()
                  Right _ -> do
                    logInfo "Deleting tokens was succeeded"
            _ -> logInfo "Deleting was cancelled"


-- Utilities

getNotDiscarded :: Ord a
  => Vector (a, b)
  -> Vector a
  -> Vector (a, b)
getNotDiscarded savedTokens discardedTokens =
  V.filter (\(x,_) -> Set.notMember x discardedTokensSet) savedTokens
    where
      discardedTokensSet = Set.fromList (V.toList discardedTokens)

addTimeLag :: NominalDiffTime -> UTCTime -> POSIXTime
addTimeLag diff utc = utcTimeToPOSIXSeconds $ addUTCTime diff utc