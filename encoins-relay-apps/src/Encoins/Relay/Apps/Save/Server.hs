{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}



module Encoins.Relay.Apps.Save.Server where

import           Encoins.Common.Constant                (column, space)
import           Encoins.Common.Log                     (logDebug, logDebugS,
                                                         logError, logErrorS,
                                                         logInfo, logWarn)
import           Encoins.Common.Transform               (toText)
import           Encoins.Common.Version                 (appVersion,
                                                         showAppVersion)
import           Encoins.Relay.Apps.Save.Config
import           Encoins.Relay.Apps.Save.Database.Query (insertOnAbsentS)
import           Encoins.Relay.Apps.Save.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro          (AssetMintsAndBurnsData (..),
                                                         AssetMintsAndBurnsResponse (..))

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception.Safe                 (SomeException,
                                                         catchAny, tryAny)
import           Control.Monad.IO.Class                 (MonadIO (liftIO))
import           Control.Monad.Reader                   (asks)
import qualified Data.List.NonEmpty                     as NE
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.String                            (IsString (fromString))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Time                              (UTCTime, addUTCTime,
                                                         getCurrentTime)
import           Development.GitRev                     (gitCommitDate, gitHash)
import qualified Hasql.Pool                             as Pool
import qualified Network.Wai                            as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors            (CorsResourcePolicy (..),
                                                         cors,
                                                         simpleCorsResourcePolicy)
import           Paths_encoins_relay_apps               (version)
import           Servant
import           Text.Pretty.Simple                     (pPrint)


cloudServer :: IO ()
cloudServer = do
  pPrint $ showAppVersion "Save server" $ appVersion version $(gitHash) $(gitCommitDate)
  withSaveEnv $ \env -> do
    -- withAsync (rottenTokenHandler env) $ \_ -> do
    withRecovery "server" $ run (envPort env) $ app env

type ServerSaveApi =
           "ping"
              :> Get '[JSON] NoContent
      :<|> "save"
              :> ReqBody '[JSON] [SaveRequest]
              :> Post '[JSON] (Map AssetName StatusResponse)

    --  :<|> "restore"
    --           :> Capture "client_id" Text
    --           :> Get '[JSON] [RestoreResponse]

serverSaveApiProxy :: Proxy ServerSaveApi
serverSaveApiProxy = Proxy

serverSaveApi :: ServerT ServerSaveApi SaveMonad
serverSaveApi = ping
           :<|> save
          --  :<|> restore

handlerServer :: SaveEnv -> ServerT ServerSaveApi Handler
handlerServer env = hoistServer serverSaveApiProxy (liftIO . runSaveMonad env) serverSaveApi

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            }

app :: SaveEnv -> Application
app = corsWithContentType . serve serverSaveApiProxy . handlerServer

ping :: SaveMonad NoContent
ping = do
  logInfo "Ping request"
  pure NoContent

save :: [SaveRequest] -> SaveMonad (Map AssetName StatusResponse)
save reqs = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  pool <- asks envPool
  mapM_ (saveToken responseTVar pool) reqs
  liftIO $ readTVarIO responseTVar

-- Save unsaved token and return their status
saveToken :: TVar (Map AssetName StatusResponse)
  -> Pool.Pool
  -> SaveRequest
  -> SaveMonad ()
saveToken tVar pool req = do
  isFormat <- asks envFormatMessage
  logInfo ""
  logInfo "Unsaved token received"
  logDebugS isFormat req
  let assetName = ppAssetName req
  coinStatus <- checkCoinStatus assetName
  logInfo $ "Coin status" <> column <> space <> toText coinStatus
  saveStatus <- case coinStatus of
    CoinError err -> do
      logErrorS isFormat err
      pure $ MkStatusResponse SaveError
    CoinDiscarded m -> do
      logInfo "Token has been discarded and can't be rollback anymore"
      logInfo $ "The reason of discarding" <> column <> space <> m
      pure $ MkStatusResponse Discarded
    _ -> do
      let name = getAssetName $ ppAssetName req
      let secret = getEncryptedSecret $ ppSecretKey req
      eResult <- liftIO $ Pool.use pool $ insertOnAbsentS name secret
      case eResult of
        Left err -> do
          logErrorS isFormat err
          pure $ MkStatusResponse SaveError
        Right _ -> do
          logInfo "Token is saved"
          pure $ MkStatusResponse Saved
  logInfo $ "Final SaveStatus" <> column <> space <>
    toText (spStatusResponse saveStatus)
  modifyCacheResponse tVar assetName saveStatus

modifyCacheResponse :: TVar (Map AssetName StatusResponse)
  -> AssetName
  -> StatusResponse
  -> SaveMonad ()
modifyCacheResponse tVar assetName resp
  = liftIO
  $ atomically
  $ modifyTVar' tVar (Map.insert assetName resp)

checkCoinStatus :: AssetName -> SaveMonad CoinStatus
checkCoinStatus assetName = do
  isFormat <- asks envFormatMessage
  currentSymbol <- asks envCurrencySymbol
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

withRecovery :: Text -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      print $ "Exception caught in" <> space <> nameOfAction <> column <> space <> toText e
      liftIO $ threadDelay $ 5 * 1000000
      withRecovery nameOfAction action

-- Burned token is discarded in 12 hours
discardTime :: UTCTime -> UTCTime
discardTime = addUTCTime (12 * 60 * 60)
