{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Encoins.Relay.Apps.Delegation.Server where

import           Cardano.Api                            (writeFileJSON)
import           Cardano.Server.Config                  (HasCreds, decodeOrErrorFromFile)
import           Cardano.Server.Main                    (runCardanoServer)
import           Cardano.Server.Utils.Logger            (logMsg, logSmth, logger, (.<))
import           Cardano.Server.Utils.Wait              (waitTime)
import           Control.Applicative                    ((<|>))
import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.Async               (async, wait)
import           Control.Monad                          (forM, forever, void, when, (>=>))
import           Control.Monad.Catch                    (Exception, MonadCatch (catch), MonadThrow (..), SomeException, handle)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Reader                   (MonadReader (ask, local), ReaderT (..))
import           Data.ByteString                        (ByteString)
import           Data.FileEmbed                         (embedFileIfExists)
import           Data.Fixed                             (Pico)
import           Data.Function                          (on)
import           Data.Functor                           ((<&>))
import           Data.IORef                             (newIORef, readIORef)
import           Data.List                              (sortBy)
import           Data.List.Extra                        (notNull)
import           Data.Map                               (Map, filterWithKey)
import qualified Data.Map                               as Map
import           Data.Maybe                             (catMaybes, listToMaybe, mapMaybe)
import           Data.String                            (IsString (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import qualified Data.Time                              as Time
import           Encoins.Relay.Apps.Delegation.Internal (DelegConfig (..), Delegation (..), DelegationEnv (..), DelegationM (..),
                                                         Progress (..), concatIpsWithBalances, delegAddress, findDeleg,
                                                         getBalances, removeDuplicates, runDelegationM, setProgress,
                                                         setTokenBalance)
import           Encoins.Relay.Apps.Internal            (formatTime, janitorFiles, loadMostRecentFile, newProgressBar)
import           Ledger                                 (PubKeyHash)
import qualified PlutusAppsExtra.IO.Blockfrost          as Bf
import           PlutusAppsExtra.Utils.Address          (addressToBech32)
import           Servant                                (Get, JSON, ReqBody, err404, err500, throwError, type (:<|>) ((:<|>)),
                                                         (:>))
import           Servant.Server.Internal.ServerError    (ServerError (..))
import           System.Directory                       (createDirectoryIfMissing)
import           System.ProgressBar                     (incProgress)
import           Text.Read                              (readMaybe)

runDelegationServer :: FilePath -> IO ()
runDelegationServer delegConfigFp = do
    DelegConfig{..} <- decodeOrErrorFromFile delegConfigFp
    progressRef <- initProgress cDelegationFolder >>= newIORef
    tokenBalanceRef <- do
        t <- Time.getCurrentTime
        b <- getBalances cNetworkId cDelegationCurrencySymbol cDelegationTokenName
        newIORef (b, t)
    let ?creds    = creds
    let env = DelegationEnv
            logger
            (Just "delegationServer.log")
            cNetworkId
            cHost
            cPort
            cHyperTextProtocol
            cDelegationFolder
            cFrequency
            cMaxDelay
            cMinTokenNumber
            cRewardTokenThreshold
            cDelegationCurrencySymbol
            cDelegationTokenName
            True
            progressRef
            tokenBalanceRef
    runDelegationServer' env

initProgress :: FilePath -> IO (Progress, Time.UTCTime)
initProgress delegFolder = initFromFile <|> initNew
    where
        initFromFile = do
            Just (t, p) <- loadMostRecentFile delegFolder "delegatorsV2_"
            T.putStrLn $ "Time of most recent progress file:" .< t
            pure (p, t)
        initNew = do
            putStrLn "No progress file found."
            pure (Progress Nothing [], Time.UTCTime (toEnum 0) 0)

runDelegationServer' :: HasCreds => DelegationEnv -> IO ()
runDelegationServer' env = do
    createDirectoryIfMissing True $ dEnvDelegationFolder env
    runCardanoServer @DelegApi env ((`runReaderT` env) . unDelegationM) delegApi beforeMainLoop
    where
        beforeMainLoop = do
            void $ liftIO $ forkIO runDelegationSerach
            logMsg "Starting delegation server..."
        runDelegationSerach
            = withRecovery
            $ runDelegationM env
            $ local (const $ env {dEnvLoggerFp = Just "delegationSearch.log"})
            $ forever searchForDelegations
        withRecovery ma = catch ma $ \(err :: SomeException) -> do
            logSmth err
            waitTime 10
            withRecovery ma

creds :: Maybe (ByteString, ByteString)
creds = let keyCred  = $(embedFileIfExists "../key.pem")
            certCred = $(embedFileIfExists "../certificate.pem")
        in (,) <$> certCred <*> keyCred

------------------------------------------------------------------- API -------------------------------------------------------------------

type DelegApi
    =    GetServers
    :<|> GetCurrentServers
    :<|> GetServerDelegators

delegApi :: DelegationM (Map Text Integer)
       :<|> DelegationM [Text]
       :<|> (Text -> DelegationM (Map Text Integer))
delegApi
    =    getServersHandler
    :<|> getCurrentServersHandler
    :<|> getServerDelegatesHandler

----------------------------------------------- Get (all) servers ips with delegated tokens number endpoint -----------------------------------------------

type GetServers = "servers" :> Get '[JSON] (Map Text Integer)
                                             -- ^ IP
getServersHandler :: DelegationM (Map Text Integer)
getServersHandler = delegationErrorH $ do
    (retiredRelays :: [Text]) <- liftIO $ decodeOrErrorFromFile "retiredRelays.json"
    filterWithKey (\k _ -> k `notElem` retiredRelays) <$> askIpsWithBalances

-------------------------------------- Get current (more than 100k(MinTokenNumber) delegated tokens) servers endpoint --------------------------------------

type GetCurrentServers = "current" :> Get '[JSON] [Text]

getCurrentServersHandler :: DelegationM [Text]
getCurrentServersHandler = delegationErrorH $ do
    DelegationEnv{..} <- ask
    ipsWithBalances   <- askIpsWithBalances
    -- We are currently using proxies for each server. DelegationMap is a map of server IPs to their proxy IPs.
    delegationMap     <- liftIO $ decodeOrErrorFromFile "delegationMap.json"

    pure $ mapMaybe (`lookup` delegationMap) $ Map.keys $ Map.filter (>= dEnvMinTokenNumber) ipsWithBalances

------------------------------------------------------ Get server delegators endpoint ------------------------------------------------------

type GetServerDelegators = "delegates" :> ReqBody '[JSON] Text :> Get '[JSON] (Map Text Integer)
                                                                                -- ^ Address
-- Get a threshold-limited list of delegate addresses with number of their tokens
getServerDelegatesHandler :: Text -> DelegationM (Map Text Integer)
getServerDelegatesHandler ip = delegationErrorH $ do
        DelegationEnv{..} <- ask
        Progress _ delegs <- askProgress
        let delegs' = sortBy (compare `on` delegCreated) $ filter ((== ip) . delegIp) delegs
        when (null delegs') $ throwM UnknownIp
        balances <- askTokenBalance <&> Map.filterWithKey (\pkh _ -> pkh `elem` fmap delegStakeKey delegs')
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
    | UnknownIp
    deriving (Show, Read, Eq, Exception)

readDelegationServerError :: Text -> Maybe DelegationServerError
readDelegationServerError "Unknown IP." = Just UnknownIp
readDelegationServerError txt = T.stripPrefix "The distribution of delegates is not yet ready - " >=> readMaybe . T.unpack $ txt

showDelegationServerError :: DelegationServerError -> String
showDelegationServerError UnknownIp = "Unknown IP."
showDelegationServerError err = ("The distribution of delegates is not yet ready - " <>) $ show err

delegationErrorH :: DelegationM a -> DelegationM a
delegationErrorH = handle $ \case
        err@(StaleProgressFile _ _) -> throwServerError err500 err
        UnknownIp                   -> throwServerError err404 UnknownIp
    where
        throwServerError servantErr serverErr = throwError servantErr{errBody = fromString $ showDelegationServerError serverErr}

------------------------------------------------------------------ Helpers ------------------------------------------------------------------

askProgress :: DelegationM Progress
askProgress = do
    ct <- liftIO Time.getCurrentTime
    DelegationEnv{..} <- ask
    (progress, progressTime) <- liftIO $ readIORef dEnvProgress
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct progressTime)
    when (diff > fromIntegral dEnvMaxDelay) $ do
        logMsg $ "Time of last prgress update:" .< progressTime
        throwM $ StaleProgressFile diff (diff - fromIntegral dEnvMaxDelay)
    pure progress

askTokenBalance :: DelegationM (Map PubKeyHash Integer)
askTokenBalance = do
    ct <- liftIO Time.getCurrentTime
    DelegationEnv{..} <- ask
    (balance, balanceTime) <- liftIO $ readIORef dEnvTokenBalance
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct balanceTime)
    when (diff > fromIntegral dEnvMaxDelay) $ logMsg $ "Time of last token balance update:" .< balanceTime
    pure balance

askIpsWithBalances :: DelegationM (Map Text Integer)
askIpsWithBalances = concatIpsWithBalances <$> do
    Progress _ delegs <- askProgress
    balances          <- askTokenBalance
    pure $ mapMaybe (\Delegation{..} -> Map.lookup delegStakeKey balances <&> (delegIp,)) delegs

searchForDelegations :: DelegationM ()
searchForDelegations = do
        DelegationEnv{..} <- ask
        ct                <- liftIO Time.getCurrentTime
        delay             <- liftIO $ async $ waitTime dEnvFrequency
        newProgress       <- updateProgress
        _                 <- updateBalances
        ipsWithBalances   <- askIpsWithBalances
        writeDeleg dEnvDelegationFolder ct newProgress
        writeResult dEnvDelegationFolder ct ipsWithBalances
        liftIO $ wait delay
    where
        writeDeleg delegFolder ct newProgress = do
            void $ liftIO $ writeFileJSON (delegFolder <> "/delegatorsV2_" <> formatTime ct <> ".json") newProgress
            janitorFiles delegFolder "delegatorsV2_"
        writeResult delegFolder ct ipsWithBalances = do
            let result = Map.map (T.pack . show) ipsWithBalances
            liftIO $ void $ writeFileJSON (delegFolder <> "/result_" <> formatTime ct <> ".json") result
            janitorFiles delegFolder "result_"

updateProgress :: DelegationM Progress
updateProgress = do
    DelegationEnv{..} <- ask
    (Progress{..}, _) <- liftIO $ readIORef dEnvProgress
    ct                <- liftIO Time.getCurrentTime
    txIds             <- liftIO $ Bf.getAllAssetTxsAfterTxId dEnvNetworkId dEnvCurrencySymbol dEnvTokenName pLastTxId
    if null txIds
    then pure Progress{..}
    else do
        pb        <- newProgressBar "Getting delegations" (length txIds)
        newDelegs <- fmap catMaybes $ forM txIds $ \txId -> do
            liftIO $ incProgress pb 1
            findDeleg txId
        when (notNull newDelegs) $ logMsg $ "New delegs:" <> foldr (prettyDeleg dEnvNetworkId) "" newDelegs
        let p = Progress (listToMaybe txIds <|> pLastTxId) $ removeDuplicates $ pDelgations <> newDelegs
        setProgress p ct
        pure p
    where
        prettyDeleg network d rest = case addressToBech32 network (delegAddress d) of
            Just addr -> rest <> "\n" <> addr <> " : " <> delegIp d
            Nothing   -> rest <> "\n" <> T.pack (show (delegCredential d) <> "<>" <> show (delegStakeKey d)) <> " : " <> delegIp d

updateBalances :: DelegationM (Map PubKeyHash Integer)
updateBalances = do
    DelegationEnv{..} <- ask
    ct <- liftIO Time.getCurrentTime
    b  <- getBalances dEnvNetworkId dEnvCurrencySymbol dEnvTokenName
    setTokenBalance b ct
    pure b