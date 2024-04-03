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
import           Encoins.Relay.Apps.Cloud.PostgreSQL.Query (getDiscardedTokensS,
                                                            getTokensS,
                                                            insertDiscardedTokensS,
                                                            insertOnAbsentS)
import           Encoins.Relay.Apps.Cloud.Types
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
import qualified Data.List.NonEmpty                        as NE
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.String                               (IsString (fromString))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import           Data.Time                                 (UTCTime, addUTCTime,
                                                            secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX                     (POSIXTime,
                                                            getPOSIXTime,
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
import           Paths_encoins_relay_apps                  (version)
import           Servant

-- import           PlutusAppsExtra.IO.Maestro                (getAccountAddressesHoldingAssets)
-- import           Data.String                               (IsString (fromString))

cloudServer :: IO ()
cloudServer = do
  B.putStrLn
    $ TE.encodeUtf8
    $ showAppVersion "Save server" $ appVersion version $(gitHash) $(gitCommitDate)
  withCloudEnv $ \env -> do
    withAsync (selectDiscardedTokens env) $ \_ -> do
      withRecovery "server" $ run (envPort env) $ app env

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
  -- logInfo $ "Secret length" <> column <> space <> toText (T.length $ getEncryptedSecret secret)
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

-- Add 12 hours
addTimeLag :: UTCTime -> POSIXTime
addTimeLag = utcTimeToPOSIXSeconds . addUTCTime timeLag

-- Lag period is 12 hours
-- After the lag token can't be rolled back
timeLag :: POSIXTime
timeLag = secondsToNominalDiffTime (12 * 60 * 60)
-- timeLag = secondsToNominalDiffTime (1 * 1 * 60)

restore :: CloudMonad (Vector (Text, Text))
restore = do
  isFormat <- asks envFormatMessage
  pool <- asks envPool
  logInfo ""
  logInfo "Restore query received"
  evTokens <- liftIO $ Pool.use pool getTokensS
  case evTokens of
    Left err -> do
      logErrorS isFormat err
      pure V.empty
    Right tokens -> do
      logInfo $ "Restored"
        <> space <> toText (V.length tokens)
        <> space <> "tokens"
      pure tokens


selectDiscardedTokens :: CloudEnv -> IO ()
selectDiscardedTokens env = withRecovery "detectDiscardedTokens" $ runCloudMonad env $ forever $ do
  isFormat <- asks envFormatMessage
  pool <- asks envPool

  logInfo ""
  logInfo "Run detectDiscardedTokens"
  evTokens <- liftIO $ Pool.use pool getDiscardedTokensS
  case evTokens of
    Left err -> do
      logErrorS isFormat err
    Right tokens -> do
      time <- liftIO $ getPOSIXTime
      discardedTokens <- V.mapMaybeM (detectDiscarded time) tokens
      -- logInfo ""
      -- logInfo "Discarded tokens"
      -- logInfoS isFormat discardedTokens
      res <- liftIO $ Pool.use pool $ insertDiscardedTokensS discardedTokens
      -- logInfo ""
      -- logInfo "Insert discarded"
      logInfoS isFormat res

  -- Sleep for 12 hour (in microseconds)
  liftIO $ threadDelay $ 12 * 60 * 60 * 1000000
  -- liftIO $ threadDelay $ 1 * 1 * 60 * 1000000


detectDiscarded :: POSIXTime
  -> (AssetName, POSIXTime)
  -> CloudMonad (Maybe (AssetName, POSIXTime))
detectDiscarded now asset@(assetName, saveTime) = do
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
      let mAsset = case assetResponses of
            [] -> if saveTime + timeLag < now
              then Just assetName
              else Nothing
            _ -> if all (isDiscarded now asset) assetResponses
              then Just assetName
              else Nothing
      logDebug $ assetNameT <> space
        <> maybe "is alive" (const "is discarded") mAsset
      pure $ (,now) <$> mAsset


isDiscarded :: POSIXTime
  -> (AssetName, POSIXTime)
  -> AssetMintsAndBurnsResponse
  -> Bool
isDiscarded now (_, saveTime) assetResponse =
  let mAssetStates = NE.nonEmpty $ ambrData assetResponse
  in case mAssetStates of
    Nothing -> saveTime + timeLag < now
    Just states ->
      let state = NE.last $ NE.sortWith ambrSlot states
      in case ambrAmount state of
          (-1) -> addTimeLag (ambrTimestamp state) < now
          _    -> False
