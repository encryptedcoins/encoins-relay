{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE NumericUnderscores #-}

module Encoins.Relay.Apps.Delegation.Server where

import           Cardano.Api                            (writeFileJSON)
import           Cardano.Server.Config                  (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Utils.Logger            (logMsg, logger, (.<))
import           Cardano.Server.Utils.Wait              (waitTime)
import           Control.Applicative                    ((<|>))
import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.Async               (async, wait)
import           Control.Monad                          (MonadPlus (mzero), forever, void, when, (>=>))
import           Control.Monad.Catch                    (Exception, MonadThrow (..), handle)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Reader                   (MonadReader (local, ask), ReaderT (..))
import           Data.Functor                           ((<&>))
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Proxy                             (Proxy (..))
import           Data.String                            (IsString (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Time                              as Time

import           Control.Arrow                          (Arrow ((&&&)))
import           Data.Maybe                             (listToMaybe)
import           Encoins.Relay.Apps.Delegation.Internal (DelegConfig (..), Delegation (delegIp, delegStakeKey, delegTxOutRef),
                                                         DelegationEnv (..), DelegationM (..), Progress (Progress, pDelgations),
                                                         getBalances, getIpsWithBalances, runDelegationM, updateProgress)
import qualified Encoins.Relay.Apps.Delegation.V1.Main  as V1
import           Encoins.Relay.Apps.Internal            (formatTime, loadMostRecentFile)
import           Encoins.Relay.Server.Config            (EncoinsRelayConfig (cDelegationCurrencySymbol, cDelegationTokenName))
import           Ledger                                 (PubKeyHash (..), TxOutRef (..))
import qualified Network.Wai.Handler.Warp               as Warp
import           Servant                                (Get, JSON, ReqBody, err404, err500, hoistServer, serve, throwError,
                                                         type (:<|>) ((:<|>)), (:>))
import           Servant.Server.Internal.ServerError    (ServerError (..))
import           System.IO                              (BufferMode (LineBuffering), hSetBuffering, stdout)
import           Text.Read                              (readMaybe)
import System.Directory (createDirectoryIfMissing)
import Data.Fixed (Pico)

runDelegationServer :: FilePath -> FilePath -> IO ()
runDelegationServer configFp delegConfigFp = do
    config          <- decodeOrErrorFromFile configFp
    relayConfig     <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
    DelegConfig{..} <- decodeOrErrorFromFile delegConfigFp
    let env = DelegationEnv
            logger
            (Just "delegationServer.log")
            (cNetworkId config)
            cHost
            cPort
            cDelegationFolder
            cFrequency
            cMaxDelay
            cMinTokenAmt
            (cDelegationCurrencySymbol relayConfig)
            (cDelegationTokenName relayConfig)
            True
    runDelegationServer' env

runDelegationServer' :: DelegationEnv -> IO ()
runDelegationServer' env = do
        hSetBuffering stdout LineBuffering
        createDirectoryIfMissing True $ dEnvDelegationFolder env
        -- Launch the delegation search application
        void $ forkIO $ setApp searchForDelegations

        runDelegationM env $ logMsg "Starting delegation server..."
        Warp.runSettings settings
            $ serve (Proxy @DelegApi)
            $ hoistServer (Proxy @DelegApi)
                ((`runReaderT` env) . unDelegationM)
                delegApi
    where
        setApp = runDelegationM env . local (const $ env {dEnvLoggerFp = Just "delegationSearch.log"}) . forever
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
       :<|> (Text -> DelegationM (Map PubKeyHash Integer))
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

-------------------------------------- Get current (more than 100k delegated tokens) servers endpoint --------------------------------------

type GetCurrentServers = "current" :> Get '[JSON] [Text]

getCurrentServersHandler :: DelegationM [Text]
getCurrentServersHandler = delegationErrorH $ do
    env <- ask
    Progress _ delegs <- getMostRecentProgressFile
    ipsWithBalances <- getIpsWithBalances delegs
    pure $ Map.keys $ Map.filter (>= dEnvMinTokenAmt env) ipsWithBalances

------------------------------------------------------ Get server delegators endpoint ------------------------------------------------------

type GetServerDelegators = "delegates" :> ReqBody '[JSON] Text :> Get '[JSON] (Map PubKeyHash Integer)

getServerDelegatesHandler :: Text -> DelegationM (Map PubKeyHash Integer)
getServerDelegatesHandler ip = delegationErrorH $ do
    Progress _ delegs <- getMostRecentProgressFile
    let delegs' = filter ((== ip) . delegIp) delegs
    when (null delegs') $ throwM UnknownIp
    getBalances <&> Map.filterWithKey (\pkh _ -> pkh `elem` fmap delegStakeKey delegs')

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
        liftIO $ V1.writeResultFile dEnvDelegationFolder ct ipsWithBalances
        liftIO $ wait delay
    where
        initProgress = do
            logMsg "No progress file found."
            pure $ Progress Nothing []

loadPastProgress :: FilePath -> IO (Time.UTCTime, Progress)
loadPastProgress delegFolder =
        (loadMostRecentFile delegFolder "delegatorsV2_" >>= maybe mzero pure)
        <|>
        (V1.loadPastProgress delegFolder >>= fromV1Progress)
    where
        fromV1Progress = maybe mzero (pure . fmap (uncurry Progress . ((listToMaybe . fmap (txOutRefId . delegTxOutRef)) &&& id)))
