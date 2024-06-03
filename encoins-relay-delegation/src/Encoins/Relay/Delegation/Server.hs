{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Encoins.Relay.Delegation.Server where

import           Cardano.Api                         (NetworkId (Mainnet), writeFileJSON)
import           Cardano.Server.Utils.Wait           (waitTime)
import           Encoins.Common.Constant             (column, newLine, space)
import           Encoins.Common.Transform            (toText)
import           Encoins.Common.Version              (appVersion, showAppVersion)
import           Encoins.Relay.Delegation.Internal   (Delegation (..), DelegationEnv (..), DelegationM (..), Progress (..), RelayAddress,
                                                      concatIpsWithBalances, delegAddress, findDeleg, fromRelayAddress, getBalances,
                                                      loadDelegationEnv, removeDuplicates, runDelegationM, setProgress, setTokenBalance,
                                                      toRelayAddress, trimIp)

import           Control.Applicative                 (liftA2, (<|>))
import           Control.Concurrent                  (forkIO)
import           Control.Concurrent.Async            (async, wait)
import           Control.Exception                   (throw)
import           Control.Monad                       (forM, forever, void, when, (>=>))
import           Control.Monad.Catch                 (Exception, MonadCatch (catch), MonadThrow (..), SomeException, handle)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Control.Monad.Reader                (MonadReader (ask), ReaderT (..), asks)
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.ByteString.Char8               as B
import           Data.Fixed                          (Pico)
import           Data.Function                       (on)
import           Data.Functor                        ((<&>))
import           Data.IORef                          (readIORef)
import           Data.List                           (find, sortBy)
import           Data.List.Extra                     (notNull, stripSuffix)
import           Data.Map                            (Map, filterWithKey)
import qualified Data.Map                            as Map
import           Data.Maybe                          (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.String                         (IsString (..))
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import qualified Data.Time                           as Time
import           Development.GitRev                  (gitCommitDate, gitHash)
import           Encoins.Common.Log                  (logErrorS, logInfo, logInfoS)
import           Encoins.Relay.Common                (decodeOrErrorFromFile, formatTime, janitorFiles, newProgressBar)
import           Encoins.Relay.Common.Server         (runCardanoServer)
import           Katip
import           Ledger                              (Address, PubKeyHash)
import           Paths_encoins_relay_delegation      (version)
import qualified PlutusAppsExtra.IO.Blockfrost       as Bf
import           PlutusAppsExtra.Utils.Address       (addressToBech32, getStakeKey)
import           Say                                 (say)
import           Servant                             (Get, JSON, Post, ReqBody, err404, err500, throwError, type (:<|>) ((:<|>)), (:>))
import           Servant.Server.Internal.ServerError (ServerError (..))
import           System.Directory                    (createDirectoryIfMissing)
import qualified System.Process                      as Process
import           System.ProgressBar                  (incProgress)
import           Text.Read                           (readMaybe)

runDelegationServer :: FilePath -> IO ()
runDelegationServer delegationConfigFp = do
    B.putStrLn
      $ TE.encodeUtf8
      $ showAppVersion "Delegation server" $ appVersion version $(gitHash) $(gitCommitDate)
    config <- decodeOrErrorFromFile delegationConfigFp
    env <- loadDelegationEnv config
    runDelegationServer' env

runDelegationServer' :: DelegationEnv -> IO ()
runDelegationServer' env = do
    createDirectoryIfMissing True $ dEnvDelegationFolder env
    runCardanoServer @DelegApi
        (dEnvPort env)
        ((`runReaderT` env) . unDelegationM)
        delegApi
        beforeMainLoop
  where
    beforeMainLoop = do
        void $ liftIO $ forkIO runDelegationSerach
        say $ showAppVersion "Verifier server" $ appVersion
            version
            $(gitHash)
            $(gitCommitDate)
    runDelegationSerach
        = runDelegationM env
        $ withRecovery
        $ katipAddNamespace "Delegations search" $ forever searchForDelegations
    withRecovery ma = catch ma $ \(err :: SomeException) -> do
        logErrorS True err
        waitTime 10
        withRecovery ma

------------------------------------------------------------------- API -------------------------------------------------------------------

type DelegApi
    =    GetServers
    :<|> GetCurrentServers
    :<|> GetServerDelegators
    :<|> GetDelegationInfo

delegApi :: DelegationM (Map Text Integer)
       :<|> DelegationM [Text]
       :<|> (Text -> DelegationM (Map Text Integer))
       :<|> (Address -> DelegationM (Text, Integer))
delegApi
    =    getServersHandler
    :<|> getCurrentServersHandler
    :<|> getServerDelegatesHandler
    :<|> getDelegationInfoHandler

----------------------------------------------- Get (all) servers ips with delegated tokens number endpoint -----------------------------------------------

type GetServers = "servers" :> Get '[JSON] (Map Text Integer)
                                             -- ^ IP
getServersHandler :: DelegationM (Map Text Integer)
getServersHandler = delegationErrorH $ do
    !(retiredRelays :: [Text]) <- liftIO $ decodeOrErrorFromFile "retiredRelays.json"
    filterWithKey (\k _ -> k `notElem` retiredRelays) <$> askIpsWithBalances True

-------------------------------------- Get current (more than 100k(MinTokenNumber) delegated tokens) servers endpoint --------------------------------------

type GetCurrentServers = "current" :> Get '[JSON] [Text]

getCurrentServersHandler :: DelegationM [Text]
getCurrentServersHandler = delegationErrorH $ do
    MkDelegationEnv{..} <- ask
    ipsWithBalances   <- askIpsWithBalances True
    !delegationMap    <- case dEnvNetworkId of
        Mainnet -> liftIO $ decodeOrErrorFromFile "delegationMap.json"
        _       -> pure Map.empty
    pure $ fmap (toProxy delegationMap dEnvNetworkId) $ Map.keys $ Map.filter (>= dEnvMinTokenNumber) ipsWithBalances
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
getServerDelegatesHandler :: Text -> DelegationM (Map Text Integer)
getServerDelegatesHandler (trimIp -> ip) = delegationErrorH $ do
        MkDelegationEnv{..} <- ask
        Progress _ delegs <- askProgress True
        let delegs' = sortBy (compare `on` delegCreated) $ filter ((== ip) . trimIp . delegIp) delegs
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

------------------------------------------- Get specific delegation info by address endpoint -------------------------------------------

type GetDelegationInfo = "info" :> ReqBody '[JSON] Address :> Post '[JSON] (Text, Integer)

-- Get ip of delegated server and number of tokens by address endpoint
getDelegationInfoHandler :: Address -> DelegationM (Text, Integer)
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
showDelegationServerError err =
  "The distribution of delegates is not yet ready -" <> space <> (show err)

delegationErrorH :: DelegationM a -> DelegationM a
delegationErrorH = handle $ \case
        err@(StaleProgressFile _ _) -> throwServerError err500 err
        UnknownIp                   -> throwServerError err404 UnknownIp
        UnknownAddress              -> throwServerError err404 UnknownAddress
    where
        throwServerError servantErr serverErr = throwError servantErr{errBody = fromString $ showDelegationServerError serverErr}

------------------------------------------------------------------ Helpers ------------------------------------------------------------------

askProgress :: Bool -> DelegationM Progress
askProgress checkProgress = do
    ct <- liftIO Time.getCurrentTime
    MkDelegationEnv{..} <- ask
    (progress, progressTime) <- liftIO $ readIORef dEnvProgress
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct progressTime)
    when (checkProgress && diff > fromIntegral dEnvMaxDelay) $ do
        logInfo "Time of last progress update:"
        logInfoS dEnvFormatMessage progressTime
        throwM $ StaleProgressFile diff (diff - fromIntegral dEnvMaxDelay)
    pure progress

askTokenBalance :: DelegationM (Map PubKeyHash Integer)
askTokenBalance = do
    ct <- liftIO Time.getCurrentTime
    MkDelegationEnv{..} <- ask
    (balance, balanceTime) <- liftIO $ readIORef dEnvTokenBalance
    let diff = Time.nominalDiffTimeToSeconds (Time.diffUTCTime ct balanceTime)
    when (diff > fromIntegral dEnvMaxDelay) $ do
        logInfo "Time of last token balance update:"
        logInfoS dEnvFormatMessage balanceTime
    pure balance

askIpsWithBalances :: Bool -> DelegationM (Map Text Integer)
askIpsWithBalances checkProgress = do
    Progress _ delegs <- askProgress checkProgress
    balances          <- askTokenBalance
    pure $ concatIpsWithBalances $ mapMaybe (\Delegation{..} -> Map.lookup delegStakeKey balances <&> (trimIp delegIp,)) delegs

searchForDelegations :: DelegationM ()
searchForDelegations = do
    MkDelegationEnv{..}  <- ask
    ct                 <- liftIO Time.getCurrentTime
    delay              <- liftIO $ async $ waitTime dEnvFrequency
    newProgress        <- updateProgress
    _                  <- updateBalances
    newIpsWithBalances <- askIpsWithBalances False
    _ <- updateDelegationMap $ Map.mapKeys toRelayAddress newIpsWithBalances
    writeDeleg dEnvDelegationFolder ct newProgress
    writeResult dEnvDelegationFolder ct newIpsWithBalances
    liftIO $ wait delay
  where
    writeDeleg delegFolder ct newProgress = do
        void $ liftIO $ writeFileJSON (delegFolder <> "/delegatorsV2_" <> formatTime ct <> ".json") newProgress
        janitorFiles delegFolder "delegatorsV2_"
    writeResult delegFolder ct ipsWithBalances = do
        let result = Map.map toText ipsWithBalances
        liftIO $ void $ writeFileJSON (delegFolder <> "/result_" <> formatTime ct <> ".json") result
        janitorFiles delegFolder "result_"

updateDelegationMap :: Map RelayAddress Integer -> DelegationM ()
updateDelegationMap relaysWithBalances = do
    !delegationMap <- liftIO $ decodeOrErrorFromFile "delegationMap.json"
    !retiredRelays <- liftIO $ decodeOrErrorFromFile "retiredRelays.json" <&> map toRelayAddress
    MkDelegationEnv{..} <- ask
    let activeRelays = filter (`notElem` retiredRelays) $ Map.keys $ Map.filter (>= dEnvMinTokenNumber) relaysWithBalances
        relaysToAdd = filter (`notElem` Map.keys delegationMap) activeRelays
        proxiesToRemove = Map.elems $ Map.filterWithKey (\r _ -> r `notElem` activeRelays) delegationMap
    proxiesToAdd <- mapM createAmazonProxy relaysToAdd
    mapM_ removeAmazonProxy proxiesToRemove
    let delegationMap' = Map.fromList (zip relaysToAdd proxiesToAdd) <> Map.filterWithKey (\k _ -> k `elem` activeRelays) delegationMap
    void $ liftIO $ writeFileJSON "delegationMap.json" delegationMap'
  where
    createAmazonProxy relay = do
        amazonID <- fmap trimAmazonID $ liftIO $ execute $ "./api-create.sh" <> space <> T.unpack (fromRelayAddress relay)
        logInfo $ "New amazon id"
          <> space
          <> T.pack amazonID
          <> space
          <> "for address"
          <> space
          <> fromRelayAddress relay
        pure $ ProxyAddress $ "https://" <> T.pack amazonID <> ".execute-api.eu-central-1.amazonaws.com/"
    trimAmazonID amazonID = fromMaybe amazonID $ stripSuffix newLine amazonID
    amazonIDfromAddress = (\a -> fromMaybe a $ T.stripSuffix ".execute-api.eu-central-1.amazonaws.com/" a)
                        . (\a -> fromMaybe a $ T.stripPrefix "https://" a)
                        . unProxyAddress
    removeAmazonProxy addr = do
        format <- asks dEnvFormatMessage
        logInfo "Removing proxy "
        logInfoS format addr
        liftIO $ execute $ "./api-delete.sh" <> space <> T.unpack (amazonIDfromAddress addr)
    execute cmd = Process.readCreateProcess ((Process.shell cmd) {Process.cwd = Just "../../scripts/delegationProxy"}) ""

newtype ProxyAddress = ProxyAddress {unProxyAddress :: Text}
    deriving newtype (Show, FromJSON, ToJSON)

updateProgress :: DelegationM Progress
updateProgress = do
    MkDelegationEnv{..} <- ask
    (Progress{..}, _) <- liftIO $ readIORef dEnvProgress
    ct                <- liftIO Time.getCurrentTime
    txIds             <- Bf.getAllAssetTxsAfterTxId dEnvCurrencySymbol dEnvTokenName pLastTxId
    if null txIds
    then logInfo "No new delegations." >> setProgress Progress{..} ct >> pure Progress{..}
    else do
        pb        <- newProgressBar "Getting delegations" (length txIds)
        newDelegs <- fmap catMaybes $ forM txIds $ \txId -> do
            liftIO $ incProgress pb 1
            findDeleg txId
        when (notNull newDelegs)
            $ logInfo $ "New delegations:" <> foldr (prettyDeleg dEnvNetworkId) "" newDelegs
        let p = Progress (listToMaybe txIds <|> pLastTxId) $ removeDuplicates $ pDelgations <> newDelegs
        setProgress p ct
        pure p
  where
    prettyDeleg network d rest = case addressToBech32 network (delegAddress d) of
        Just addr -> rest <> newLine <> addr <> space <> column <> space <> trimIp (delegIp d)
        Nothing   -> rest
          <> newLine <> toText (delegCredential d)
          <> "<>" <> toText (delegStakeKey d)
          <> space <> column
          <> space <> trimIp (delegIp d)

updateBalances :: DelegationM (Map PubKeyHash Integer)
updateBalances = do
    MkDelegationEnv{..} <- ask
    ct <- liftIO Time.getCurrentTime
    b  <- getBalances dEnvCurrencySymbol dEnvTokenName
    setTokenBalance b ct
    pure b
