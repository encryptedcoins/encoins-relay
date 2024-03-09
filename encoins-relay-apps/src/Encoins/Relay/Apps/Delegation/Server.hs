{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Encoins.Relay.Apps.Delegation.Server where

import           Cardano.Api                            (NetworkId (Mainnet), writeFileJSON)
import           Cardano.Server.Config                  (AuxillaryConfigOf, decodeOrErrorFromFile)
import           Cardano.Server.Internal                (AppT, AuxillaryEnvOf, ServerM, envAuxilaryEnv, getAuxillaryEnv, loadEnv,
                                                         setLoggerFilePath)
import           Cardano.Server.Main                    (RunSettings (..), runServer)
import           Cardano.Server.Utils.Logger            (logMsg, logSmth, (.<))
import           Cardano.Server.Utils.Wait              (waitTime)
import           Control.Applicative                    (liftA2, (<|>))
import           Control.Concurrent.Async               (async, wait)
import           Control.Exception                      (throw)
import           Control.Monad                          (forM, forever, void, when, (>=>))
import           Control.Monad.Catch                    (Exception, MonadCatch (catch), MonadThrow (..), SomeException, handle)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.ByteString                        (ByteString)
import           Data.Default                           (Default (..))
import           Data.FileEmbed                         (embedFileIfExists)
import           Data.Fixed                             (Pico)
import           Data.Function                          (on)
import           Data.Functor                           ((<&>))
import           Data.IORef                             (readIORef)
import           Data.List                              (find, sortBy)
import           Data.List.Extra                        (notNull, stripSuffix)
import           Data.Map                               (Map, filterWithKey)
import qualified Data.Map                               as Map
import           Data.Maybe                             (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.String                            (IsString (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Time                              as Time
import           Encoins.Relay.Apps.Delegation.Internal (Delegation (..), DelegationConfig (..), DelegationEnv (..), Progress (..),
                                                         RelayAddress, concatIpsWithBalances, delegAddress, findDeleg, fromRelayAddress,
                                                         getBalances, loadDelegationEnv, removeDuplicates, setProgress, setTokenBalance,
                                                         toRelayAddress, trimIp)
import           Encoins.Relay.Apps.Internal            (formatTime, janitorFiles, newProgressBar)
import           Ledger                                 (Address, PubKeyHash)
import           PlutusAppsExtra.Api.Blockfrost         (MonadBlockfrost)
import qualified PlutusAppsExtra.IO.Blockfrost          as Bf
import           PlutusAppsExtra.Utils.Address          (addressToBech32, getStakeKey)
import           Servant                                (Get, JSON, Post, ReqBody, err404, err500, throwError, type (:<|>) ((:<|>)), (:>))
import qualified Servant
import           Servant.Server.Internal.ServerError    (ServerError (..))
import           System.Directory                       (createDirectoryIfMissing)
import qualified System.Process                         as Process
import           System.ProgressBar                     (incProgress)
import           Text.Read                              (readMaybe)
import           UnliftIO.Concurrent                    (forkIO)

runDelegationServer :: FilePath -> IO ()
runDelegationServer delegConfigFp = do
    config <- decodeOrErrorFromFile delegConfigFp
    DelegationConfig{..} <- decodeOrErrorFromFile delegConfigFp
    let ?creds = creds
    auxEnv <- loadDelegationEnv DelegationConfig{..}
    env <- loadEnv config auxEnv
    createDirectoryIfMissing True $ envDelegationFolder $ envAuxilaryEnv env
    runServer @DelegApi delegationServer env delegationRunSettings

delegationRunSettings :: RunSettings DelegApi
delegationRunSettings = def
    { rsBeforeMainLoop = void $ forkIO runDelegationSerach
    , rsServerName     = "delegation server"
    }
  where
    runDelegationSerach
        = withRecovery
        $ setLoggerFilePath "delegationSearch.log"
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
    :<|> GetDelegationInfo

delegationServer :: Servant.ServerT DelegApi (ServerM DelegApi)
delegationServer
    =    getServersHandler
    :<|> getCurrentServersHandler
    :<|> getServerDelegatesHandler
    :<|> getDelegationInfoHandler

type instance AuxillaryConfigOf DelegApi = DelegationConfig
type instance AuxillaryEnvOf    DelegApi = DelegationEnv

----------------------------------------------- Get (all) servers ips with delegated tokens number endpoint -----------------------------------------------

type GetServers = "servers" :> Get '[JSON] (Map Text Integer)
                                             -- ^ IP
getServersHandler :: ServerM DelegApi (Map Text Integer)
getServersHandler = delegationErrorH $ do
    !(retiredRelays :: [Text]) <- liftIO $ decodeOrErrorFromFile "retiredRelays.json"
    filterWithKey (\k _ -> k `notElem` retiredRelays) <$> askIpsWithBalances True

-------------------------------------- Get current (more than 100k(MinTokenNumber) delegated tokens) servers endpoint --------------------------------------

type GetCurrentServers = "current" :> Get '[JSON] [Text]

getCurrentServersHandler :: ServerM DelegApi [Text]
getCurrentServersHandler = delegationErrorH $ do
    DelegationEnv{..} <- getAuxillaryEnv
    ipsWithBalances   <- askIpsWithBalances True
    !delegationMap    <- case envNetworkId of
        Mainnet -> liftIO $ decodeOrErrorFromFile "delegationMap.json"
        _       -> pure Map.empty
    pure $ fmap (toProxy delegationMap envNetworkId) $ Map.keys $ Map.filter (>= envMinTokenNumber) ipsWithBalances
    where
        -- We are currently using proxies for each server. DelegationMap is a map of server IPs to their proxy IPs.
        toProxy :: Map Text Text -> NetworkId -> Text -> Text
        toProxy delegationMap network ip = case network of
            Mainnet -> fromMaybe ip $ Map.lookup ("http://" <> ip) delegationMap
            _       -> ip

------------------------------------------------------ Get server delegators endpoint ------------------------------------------------------

type GetServerDelegators = "delegates" :> ReqBody '[JSON] Text :> Get '[JSON] (Map Text Integer)
                                                                                -- ^ Address
-- Get a threshold-limited list of delegate addresses with number of their tokens
getServerDelegatesHandler :: Text -> ServerM DelegApi (Map Text Integer)
getServerDelegatesHandler (trimIp -> ip) = delegationErrorH $ do
        DelegationEnv{..} <- getAuxillaryEnv
        Progress _ delegs <- askProgress True
        let delegs' = sortBy (compare `on` delegCreated) $ filter ((== ip) . trimIp . delegIp) delegs
        when (null delegs') $ throwM UnknownIp
        balances <- askTokenBalance <&> Map.filterWithKey (\pkh _ -> pkh `elem` fmap delegStakeKey delegs')
        let delegsWithBalances = filterWithThreshold envRewardTokenThreshold $ addBalance balances delegs'
            addrsWithBalances = mapMaybe (\(d, b) -> (,b) <$> addressToBech32 envNetworkId (delegAddress d)) delegsWithBalances
        pure $ Map.fromList addrsWithBalances
    where
        addBalance balances = mapMaybe (\d -> (d,) <$> Map.lookup (delegStakeKey d) balances)
        filterWithThreshold _ [] = []
        filterWithThreshold threshold ((del, balance) : ds)
            | threshold <= 0       = []
            | balance <= threshold = (del, balance) : filterWithThreshold (threshold - balance) ds
            | otherwise            = [(del, threshold)]

------------------------------------------- Get specific delegation info by address endpoint -------------------------------------------

type GetDelegationInfo = "info" :> ReqBody '[JSON] Address :> Post '[JSON] (Text, Integer)

-- Get ip of delegated server and number of tokens by address endpoint
getDelegationInfoHandler :: Address -> ServerM DelegApi (Text, Integer)
getDelegationInfoHandler addr = delegationErrorH $ do
    let pkh = fromMaybe (throw err404) $ getStakeKey addr
    mbIp      <- fmap (trimIp . delegIp) . find ((== pkh) . delegStakeKey) . pDelgations <$> askProgress True
    mbBalance <- Map.lookup pkh <$> askTokenBalance
    maybe (throwM UnknownAddress) pure $ liftA2 (,) mbIp mbBalance

------------------------------------------------------------------- Errors -------------------------------------------------------------------

data DelegationServerError
    = StaleProgressFile Pico Pico
    -- ^ The maximum allowable difference and how much it was exceeded, in seconds
    | UnknownIp
    | UnknownAddress
    deriving (Show, Read, Eq, Exception)

readDelegationServerError :: Text -> Maybe DelegationServerError
readDelegationServerError "Unknown IP."  = Just UnknownIp
readDelegationServerError "Unknown wallet address." = Just UnknownAddress
readDelegationServerError txt = T.stripPrefix "The distribution of delegates is not yet ready - " >=> readMaybe . T.unpack $ txt

showDelegationServerError :: DelegationServerError -> String
showDelegationServerError UnknownIp  = "Unknown IP."
showDelegationServerError UnknownAddress = "Unknown wallet address."
showDelegationServerError err = ("The distribution of delegates is not yet ready - " <>) $ show err

delegationErrorH :: ServerM DelegApi a -> ServerM DelegApi a
delegationErrorH = handle $ \case
        err@(StaleProgressFile _ _) -> throwServerError err500 err
        UnknownIp                   -> throwServerError err404 UnknownIp
        UnknownAddress              -> throwServerError err404 UnknownAddress
    where
        throwServerError servantErr serverErr = throwError servantErr{errBody = fromString $ showDelegationServerError serverErr}

------------------------------------------------------------------ Helpers ------------------------------------------------------------------

askProgress :: (MonadIO m, MonadThrow m) => Bool -> AppT DelegApi m Progress
askProgress checkProgress = do
    ct <- liftIO Time.getCurrentTime
    DelegationEnv{..} <- getAuxillaryEnv
    (progress, progressTime) <- liftIO $ readIORef envProgress
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct progressTime)
    when (checkProgress && diff > fromIntegral envMaxDelay) $ do
        logMsg $ "Time of last progress update:" .< progressTime
        throwM $ StaleProgressFile diff (diff - fromIntegral envMaxDelay)
    pure progress

askTokenBalance :: MonadIO m => AppT DelegApi m (Map PubKeyHash Integer)
askTokenBalance = do
    ct <- liftIO Time.getCurrentTime
    DelegationEnv{..} <- getAuxillaryEnv
    (balance, balanceTime) <- liftIO $ readIORef envTokenBalance
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct balanceTime)
    when (diff > fromIntegral envMaxDelay) $ logMsg $ "Time of last token balance update:" .< balanceTime
    pure balance

askIpsWithBalances :: (MonadIO m, MonadThrow m) => Bool -> AppT DelegApi m (Map Text Integer)
askIpsWithBalances checkProgress = do
    Progress _ delegs <- askProgress checkProgress
    balances          <- askTokenBalance
    pure $ concatIpsWithBalances $ mapMaybe (\Delegation{..} -> Map.lookup delegStakeKey balances <&> (trimIp delegIp,)) delegs

searchForDelegations :: AppT DelegApi IO ()
searchForDelegations = do
        DelegationEnv{..}  <- getAuxillaryEnv
        ct                 <- liftIO Time.getCurrentTime
        delay              <- liftIO $ async $ waitTime envFrequency
        newProgress        <- updateProgress
        _                  <- updateBalances
        newIpsWithBalances <- askIpsWithBalances False
        _ <- updateDelegationMap $ Map.mapKeys toRelayAddress newIpsWithBalances
        writeDeleg envDelegationFolder ct newProgress
        writeResult envDelegationFolder ct newIpsWithBalances
        liftIO $ wait delay
    where
        writeDeleg delegFolder ct newProgress = do
            void $ liftIO $ writeFileJSON (delegFolder <> "/delegatorsV2_" <> formatTime ct <> ".json") newProgress
            janitorFiles delegFolder "delegatorsV2_"
        writeResult delegFolder ct ipsWithBalances = do
            let result = Map.map (T.pack . show) ipsWithBalances
            liftIO $ void $ writeFileJSON (delegFolder <> "/result_" <> formatTime ct <> ".json") result
            janitorFiles delegFolder "result_"

updateDelegationMap :: Map RelayAddress Integer -> AppT DelegApi IO ()
updateDelegationMap relaysWithBalances = do
        !delegationMap <- liftIO $ decodeOrErrorFromFile "delegationMap.json"
        !retiredRelays <- liftIO $ decodeOrErrorFromFile "retiredRelays.json" <&> map toRelayAddress
        DelegationEnv{..} <- getAuxillaryEnv
        let activeRelays = filter (`notElem` retiredRelays) $ Map.keys $ Map.filter (>= envMinTokenNumber) relaysWithBalances
            relaysToAdd = filter (`notElem` Map.keys delegationMap) activeRelays
            proxiesToRemove = Map.elems $ Map.filterWithKey (\r _ -> r `notElem` activeRelays) delegationMap
        proxiesToAdd <- mapM createAmazonProxy relaysToAdd
        mapM_ removeAmazonProxy proxiesToRemove
        let delegationMap' = Map.fromList (zip relaysToAdd proxiesToAdd) <> Map.filterWithKey (\k _ -> k `elem` activeRelays) delegationMap
        void $ liftIO $ writeFileJSON "delegationMap.json" delegationMap'
    where
        createAmazonProxy relay = do
            amazonID <- fmap trimAmazonID $ liftIO $ execute $ "./api-create.sh " <> T.unpack (fromRelayAddress relay)
            logMsg $ "New amazon id " <> T.pack amazonID <> " for address " <> fromRelayAddress relay
            pure $ ProxyAddress $ "https://" <> T.pack amazonID <> ".execute-api.eu-central-1.amazonaws.com/"
        trimAmazonID amazonID = fromMaybe amazonID $ stripSuffix "\n" amazonID
        amazonIDfromAddress = (\a -> fromMaybe a $ T.stripSuffix ".execute-api.eu-central-1.amazonaws.com/" a)
                            . (\a -> fromMaybe a $ T.stripPrefix "https://" a)
                            . unProxyAddress
        removeAmazonProxy addr = do
            logMsg $ "Removing proxy " .< show addr
            liftIO $ execute $ "./api-delete.sh " <> T.unpack (amazonIDfromAddress addr)
        execute cmd = Process.readCreateProcess ((Process.shell cmd) {Process.cwd = Just "../../scripts/delegationProxy"}) ""

newtype ProxyAddress = ProxyAddress {unProxyAddress :: Text}
    deriving newtype (Show, FromJSON, ToJSON)

updateProgress :: (MonadBlockfrost (AppT DelegApi m), MonadCatch m, MonadIO m) => AppT DelegApi m Progress
updateProgress = do
    DelegationEnv{..} <- getAuxillaryEnv
    (Progress{..}, _) <- liftIO $ readIORef envProgress
    ct                <- liftIO Time.getCurrentTime
    txIds             <- Bf.getAllAssetTxsAfterTxId envCurrencySymbol envTokenName pLastTxId
    if null txIds
    then logMsg "No new delegations." >> setProgress Progress{..} ct >> pure Progress{..}
    else do
        pb        <- newProgressBar "Getting delegations" (length txIds)
        newDelegs <- fmap catMaybes $ forM txIds $ \txId -> do
            liftIO $ incProgress pb 1
            findDeleg txId
        when (notNull newDelegs) $ logMsg $ "New delegations:" <> foldr (prettyDeleg envNetworkId) "" newDelegs
        let p = Progress (listToMaybe txIds <|> pLastTxId) $ removeDuplicates $ pDelgations <> newDelegs
        setProgress p ct
        pure p
    where
        prettyDeleg network d rest = case addressToBech32 network (delegAddress d) of
            Just addr -> rest <> "\n" <> addr <> " : " <> trimIp (delegIp d)
            Nothing   -> rest <> "\n" <> T.pack (show (delegCredential d) <> "<>" <> show (delegStakeKey d)) <> " : " <> trimIp (delegIp d)

updateBalances :: (MonadIO m, MonadCatch m) => AppT DelegApi m (Map PubKeyHash Integer)
updateBalances = do
    DelegationEnv{..} <- getAuxillaryEnv
    ct <- liftIO Time.getCurrentTime
    b  <- getBalances envCurrencySymbol envTokenName
    setTokenBalance b ct
    pure b