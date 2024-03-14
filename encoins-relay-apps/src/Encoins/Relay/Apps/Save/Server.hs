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

import           Encoins.Common.Constant        (column, dash, space)
import           Encoins.Common.Log             (logDebug, logDebugS, logError,
                                                 logErrorS, logInfo, logWarn)
import           Encoins.Common.Transform       (toJsonStrict, toText)
import           Encoins.Common.Version         (appVersion, showAppVersion)
import           Encoins.Relay.Apps.Save.Config
import           Encoins.Relay.Apps.Save.Types
import           PlutusAppsExtra.IO.Maestro
import           PlutusAppsExtra.Utils.Maestro  (AssetMintsAndBurnsData (..),
                                                 AssetMintsAndBurnsResponse (..))

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception.Safe         (SomeException, catchAny, throw,
                                                 tryAny)
import           Control.Monad                  (foldM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (asks)
import           Data.Aeson                     (eitherDecodeFileStrict')
import qualified Data.ByteString                as BS
import           Data.List.Extra                (stripInfix)
import qualified Data.List.NonEmpty             as NE
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Semigroup                 (Max (..))
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime, addUTCTime,
                                                 getCurrentTime)
import           Development.GitRev             (gitCommitDate, gitHash)
import qualified Network.Wai                    as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy (..), cors,
                                                 simpleCorsResourcePolicy)
import           Numeric.Natural                (Natural)
import           Paths_encoins_relay_apps       (version)
import           Say
import           Servant
import           System.Directory.Extra         (listFiles)
import           System.FilePath.Posix          (takeBaseName, (</>))
import           Text.Read                      (readMaybe)

saveServer :: IO ()
saveServer = do
  say $ showAppVersion "Save server" $ appVersion version $(gitHash) $(gitCommitDate)
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

-- TODO: get rid of liftIO

ping :: SaveMonad NoContent
ping = do
  logInfo "Ping request"
  pure NoContent

save :: [SaveRequest] -> SaveMonad (Map AssetName StatusResponse)
save reqs = do
  responseTVar <- liftIO $ newTVarIO Map.empty
  mapM_ (saveToken responseTVar) reqs
  liftIO $ readTVarIO responseTVar

-- Save minted tokens and return their statuses
saveToken :: TVar (Map AssetName StatusResponse)
  -> SaveRequest
  -> SaveMonad ()
saveToken tVar req = do
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
      mDup <- checkSaveStatus req
      case mDup of
        Nothing -> do
          logInfo "Token is not saved"
          let dupNum = 0
          let newToken = MkSaveToken (ppAssetName req) (ppSecretKey req) dupNum
          status <- saveTokenFile newToken dupNum
          pure $ MkStatusResponse status
        Just filterStatus -> case filterStatus of
          Exist -> do
            logInfo "The same token was saved before. No need to save it anymore"
            pure $ MkStatusResponse Saved
          FMax (getMax -> lastDuplicateNumber) -> do
            let nextDupNum = lastDuplicateNumber + 1
            logInfo $ "There are" <> space
              <> toText nextDupNum <> space
              <> "tokens that already saved with other aes keys"
            let newToken = MkSaveToken (ppAssetName req) (ppSecretKey req) nextDupNum
            status <- saveTokenFile newToken nextDupNum
            pure $ MkStatusResponse status
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

checkSaveStatus :: SaveRequest -> SaveMonad (Maybe FilterAssetStatus)
checkSaveStatus req = do
  logInfo "Check SaveStatus"
  saveDirectory <- asks envSaveDirectory
  savedFiles <- liftIO $ listFiles saveDirectory
  foldM (filterAssets req) Nothing savedFiles

-- if all are Nothing then file is absent, no matter there is error or not.
filterAssets :: SaveRequest
  -> Maybe FilterAssetStatus
  -> FilePath
  -> SaveMonad (Maybe FilterAssetStatus)
filterAssets (MkSaveRequest name secret) acc fp = do
  let bn = takeBaseName fp
  case stripInfix dash bn of
    Nothing -> do
      logError $ "Failed to strip filepath"
        <> column <> space <> toText bn
      pure $ acc <> Nothing -- Error: Invalid filepath
    Just (aName, dupNumber) -> do
      case getAssetName name == T.pack aName of -- select by name
        False -> pure $ acc <> Nothing -- Absent
        True -> case readMaybe @Natural dupNumber of -- select by number
          Nothing -> do
            logError $ "Failed to read duplicate number of filepath"
              <> column <> space <> toText bn
            pure $ acc <> Nothing -- Error: Invalid Duplicate Number
          Just n -> do
              eFile <- liftIO $ eitherDecodeFileStrict' @SaveToken fp
              case eFile of
                Left err -> do
                  logError $ T.pack err
                  pure $ acc <> Nothing -- Error: Invalid file
                Right file -> do
                  case stSecret file == secret of
                    True  -> pure $ acc <> Just Exist
                    False -> pure $ acc <> Just (FMax $ Max n)

saveTokenFile :: SaveToken -> Natural -> SaveMonad SaveStatus
saveTokenFile sToken num = do
  saveDirectory <- asks envSaveDirectory
  let mkFilePath = saveDirectory
        </> (T.unpack $ getAssetName $ stAssetName sToken)
        <> dash
        <> show num
  let tokenJson = toJsonStrict sToken
  eRes <- liftIO <$> tryAny $ BS.writeFile mkFilePath tokenJson
  case eRes of
    Left err -> do
      isFormat <- asks envFormatMessage
      logErrorS isFormat err
      pure SaveError
    Right _ -> pure Saved

withRecovery :: Text -> IO () -> IO ()
withRecovery nameOfAction action = action `catchAny` handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      say $ "Exception caught in" <> space <> nameOfAction <> column <> space <> toText e
      liftIO $ threadDelay $ 5 * 1000000
      withRecovery nameOfAction action

-- Discarding burned token in 12 hours
discardTime :: UTCTime -> UTCTime
discardTime = addUTCTime (12 * 60 * 60)
