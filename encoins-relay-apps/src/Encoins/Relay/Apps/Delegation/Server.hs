{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Encoins.Relay.Apps.Delegation.Server where

import           Cardano.Api                            (writeFileJSON)
import           Cardano.Server.Config                  (decodeOrErrorFromFile)
import           Cardano.Server.Main                    (corsWithContentType)
import           Cardano.Server.Utils.Logger            (logMsg, logger, (.<))
import           Cardano.Server.Utils.Wait              (waitTime)
import           Control.Applicative                    ((<|>))
import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.Async               (async, wait)
import           Control.Monad                          (forever, void, when, (>=>))
import           Control.Monad.Catch                    (Exception, MonadThrow (..), handle)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Reader                   (MonadReader (ask, local), ReaderT (..))
import           Data.Fixed                             (Pico)
import           Data.Function                          (on)
import           Data.Functor                           ((<&>))
import           Data.List                              (sortBy)
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (mapMaybe)
import           Data.Proxy                             (Proxy (..))
import           Data.String                            (IsString (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Time                              as Time
import           Encoins.Relay.Apps.Delegation.Internal (DelegConfig (..), Delegation (..), DelegationEnv (..), DelegationM (..),
                                                         Progress (Progress, pDelgations), delegAddress, getBalances,
                                                         getIpsWithBalances, loadPastProgress, runDelegationM, updateProgress,
                                                         writeResultFile)
import           Encoins.Relay.Apps.Internal            (formatTime)
import qualified Network.Wai.Handler.Warp               as Warp
import           PlutusAppsExtra.Utils.Address          (addressToBech32)
import           Servant                                (Get, JSON, ReqBody, err404, err500, hoistServer, serve, throwError,
                                                         type (:<|>) ((:<|>)), (:>))
import           Servant.Server.Internal.ServerError    (ServerError (..))
import           System.Directory                       (createDirectoryIfMissing)
import           System.IO                              (BufferMode (LineBuffering), hSetBuffering, stdout)
import           Text.Read                              (readMaybe)

runDelegationServer :: FilePath -> IO ()
runDelegationServer delegConfigFp = do
    DelegConfig{..} <- decodeOrErrorFromFile delegConfigFp
    let env = DelegationEnv
            logger
            (Just "delegationServer.log")
            cNetworkId
            cHost
            cPort
            cDelegationFolder
            cFrequency
            cMaxDelay
            cMinTokenNumber
            cRewardTokenThreshold
            cDelegationCurrencySymbol
            cDelegationTokenName
            True
    runDelegationServer' env

runDelegationServer' :: DelegationEnv -> IO ()
runDelegationServer' env = do
        hSetBuffering stdout LineBuffering
        createDirectoryIfMissing True $ dEnvDelegationFolder env

        -- Launch the delegation search application
        void $ forkIO runDelegationSerach

        runDelegationM env $ logMsg "Starting delegation server..."
        Warp.runSettings settings
            $ corsWithContentType
            $ serve (Proxy @DelegApi)
            $ hoistServer (Proxy @DelegApi)
                ((`runReaderT` env) . unDelegationM)
                delegApi
    where
        runDelegationSerach
            = runDelegationM env
            $ local (const $ env {dEnvLoggerFp = Just "delegationSearch.log"}) 
            $ forever searchForDelegations
        settings
            = Warp.setLogger logReceivedRequest
            $ Warp.setOnException (const logException)
            $ Warp.setHost (fromString $ T.unpack $ dEnvHost env)
            $ Warp.setPort (dEnvPort env) Warp.defaultSettings
        logReceivedRequest req status _ = runDelegationM env $
            logMsg $ "Received request:\n" .< req <> "\nStatus:" .< status
        logException e = runDelegationM env $
            logMsg $ "Unhandled exception:\n" .< e

------------------------------------------------------------------- API -------------------------------------------------------------------

type DelegApi
    =    GetServers
    :<|> GetCurrentServers
    :<|> GetServerDelegators

delegApi :: DelegationM [Text]
       :<|> DelegationM [Text]
       :<|> (Text -> DelegationM (Map Text Integer))
delegApi
    =    getServersHandler
    :<|> getCurrentServersHandler
    :<|> getServerDelegatesHandler

-------------------------------------------------------- Get (all) servers endpoint --------------------------------------------------------

type GetServers = "servers" :> Get '[JSON] [Text]

getServersHandler :: DelegationM [Text]
getServersHandler = delegationErrorH $ do
    Progress _ delegs <- getMostRecentProgressFile
    pure $ delegIp <$> delegs

-------------------------------------- Get current (more than 100k(MinTokenNumber) delegated tokens) servers endpoint --------------------------------------

type GetCurrentServers = "current" :> Get '[JSON] [Text]

getCurrentServersHandler :: DelegationM [Text]
getCurrentServersHandler = delegationErrorH $ do
    env <- ask
    Progress _ delegs <- getMostRecentProgressFile
    ipsWithBalances <- getIpsWithBalances delegs
    pure $ Map.keys $ Map.filter (>= dEnvMinTokenNumber env) ipsWithBalances

------------------------------------------------------ Get server delegators endpoint ------------------------------------------------------

type GetServerDelegators = "delegates" :> ReqBody '[JSON] Text :> Get '[JSON] (Map Text Integer)
                                                                                -- ^ Address
-- Get a threshold-limited list of delegate addresses with number of their tokens
getServerDelegatesHandler :: Text -> DelegationM (Map Text Integer)
getServerDelegatesHandler ip = delegationErrorH $ do
        DelegationEnv{..} <- ask
        Progress _ delegs <- getMostRecentProgressFile
        let delegs' = sortBy (compare `on` delegCreated) $ filter ((== ip) . delegIp) delegs
        when (null delegs') $ throwM UnknownIp
        balances <- getBalances <&> Map.filterWithKey (\pkh _ -> pkh `elem` fmap delegStakeKey delegs')
        let delegsWithBalances = filterWithThreshold dEnvRewardTokenThreshold $ addBalance balances delegs'
            addrsWithBalances = mapMaybe (\(d, b) -> (,b) <$> addressToBech32 dEnvNetworkId (delegAddress d)) delegsWithBalances
        pure $ Map.fromList addrsWithBalances
    where
        addBalance balances = mapMaybe (\d -> (d,) <$> Map.lookup (delegStakeKey d) balances)
        filterWithThreshold _ [] = []
        filterWithThreshold threshold ((del, balance) : ds)
            | threshold <= 0       = []
            | balance <= threshold = (del, balance) : filterWithThreshold (threshold - balance) ds
            | otherwise            = [(del, threshold)]

------------------------------------------------------------------- Errors -------------------------------------------------------------------

data DelegationServerError
    = StaleProgressFile Pico Pico
    -- ^ The maximum allowable difference and how much it was exceeded, in seconds
    | NoProgressFile
    | UnknownIp
    deriving (Show, Read, Eq, Exception)

delegationServerErr500Prefix :: IsString str => str
delegationServerErr500Prefix = "The distribution of delegates is not yet ready - "

readDelegationServerError :: Text -> Maybe DelegationServerError
readDelegationServerError "Unknown IP." = Just UnknownIp
readDelegationServerError txt = T.stripPrefix delegationServerErr500Prefix >=> readMaybe . T.unpack $ txt

showDelegationServerError :: DelegationServerError -> String
showDelegationServerError UnknownIp = "Unknown IP."
showDelegationServerError err = (delegationServerErr500Prefix <>) $ show err

delegationErrorH :: DelegationM a -> DelegationM a
delegationErrorH = handle $ \case
        err@(StaleProgressFile _ _) -> throwServerError err500 err
        err@NoProgressFile          -> throwServerError err500 err
        UnknownIp                   -> throwServerError err404 UnknownIp
    where
        throwServerError servantErr serverErr = throwError servantErr{errBody = fromString $ showDelegationServerError serverErr}

------------------------------------------------------------------ Helpers ------------------------------------------------------------------

getMostRecentProgressFile :: DelegationM Progress
getMostRecentProgressFile = do
    DelegationEnv{..} <- ask
    (progressTime, p) <- liftIO $ loadPastProgress dEnvDelegationFolder <|> throwM NoProgressFile
    logMsg $ "Time of most recent progress file: " .< progressTime
    ct <- liftIO Time.getCurrentTime
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct progressTime)
    when (diff > fromIntegral dEnvMaxDelay) $ throwM $ StaleProgressFile diff (diff - fromIntegral dEnvMaxDelay)
    pure p

searchForDelegations :: DelegationM ()
searchForDelegations = do
        DelegationEnv{..} <- ask
        ct                <- liftIO Time.getCurrentTime
        delay             <- liftIO $ async $ waitTime dEnvFrequency
        pastProgress      <- liftIO $ snd <$> loadPastProgress dEnvDelegationFolder <|> initProgress
        newProgress       <- updateProgress pastProgress
        void $ liftIO $ writeFileJSON (dEnvDelegationFolder <> "/delegatorsV2_" <> formatTime ct <> ".json") newProgress
        ipsWithBalances   <- getIpsWithBalances $ pDelgations newProgress
        liftIO $ writeResultFile dEnvDelegationFolder ct ipsWithBalances
        liftIO $ wait delay
    where
        initProgress = do
            logMsg "No progress file found."
            pure $ Progress Nothing []
