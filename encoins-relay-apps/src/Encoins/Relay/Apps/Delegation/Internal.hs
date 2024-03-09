{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Encoins.Relay.Apps.Delegation.Internal where

import           Cardano.Api                    (NetworkId)
import           Cardano.Server.Config          (decodeOrErrorFromFile)
import           Cardano.Server.Internal        (AppT, AuxillaryEnvOf, getAuxillaryEnv)
import           Cardano.Server.Utils.Logger    ((.<))
import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, guard, when)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans            (MonadTrans (..))
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.Aeson                     (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..),
                                                 genericParseJSON)
import           Data.Aeson.Casing              (aesonPrefix, snakeCase)
import           Data.Aeson.Types               (toJSONKeyText)
import           Data.Function                  (on)
import           Data.Functor                   ((<&>))
import           Data.IORef                     (IORef, atomicWriteIORef, newIORef)
import           Data.List                      (sortBy)
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import           Data.Ord                       (Down (..))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import qualified Data.Time                      as Time
import           Encoins.Relay.Apps.Internal    (loadMostRecentFile)
import           GHC.Generics                   (Generic)
import           Ledger                         (Address (..), Credential, Datum (..), DatumFromQuery (..), PubKeyHash (..), Slot,
                                                 TxId (..), TxOutRef (..))
import           Network.URI                    (isIPv4address, isURI)
import           Plutus.V1.Ledger.Api           (Credential (..), CurrencySymbol, FromData (..), StakingCredential (..), TokenName,
                                                 fromBuiltin)
import           PlutusAppsExtra.Api.Blockfrost (BlockfrostToken, MonadBlockfrost (..))
import           PlutusAppsExtra.Api.Maestro    (MaestroToken, MonadMaestro (..))
import qualified PlutusAppsExtra.IO.Blockfrost  as Bf
import qualified PlutusAppsExtra.IO.Maestro     as Maestro
import           PlutusAppsExtra.Utils.Address  (getStakeKey)
import           PlutusAppsExtra.Utils.Maestro  (TxDetailsOutput (..), TxDetailsResponse (..))
import           PlutusTx.Builtins              (decodeUtf8)
import           Text.Read                      (readMaybe)

data DelegationConfig = DelegationConfig
    { cNetworkId                :: NetworkId
    , cBlockfrostTokenFilePath  :: Maybe FilePath
    , cMaestroTokenFilePath     :: Maybe FilePath
    , cDelegationCurrencySymbol :: CurrencySymbol
    , cDelegationTokenName      :: TokenName
    , cDelegationFolder         :: FilePath
    , cFrequency                :: Int
    -- ^ Minimal frequency of search for new delegations in seconds
    , cMaxDelay                 :: Int
    -- ^ Maximum permissible synchronization delay in seconds, if exceeded, an error will be thrown
    , cMinTokenNumber           :: Integer
    -- ^ The number of tokens, exceeding which the server gets into the current servers endpoint
    , cRewardTokenThreshold     :: Integer
    -- ^ The number of tokens that limits the distribution of rewards
    } deriving (Show, Generic)

instance FromJSON DelegationConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data DelegationEnv = DelegationEnv
    { envNetworkId            :: NetworkId
    , envMaestroToken         :: Maybe MaestroToken
    , envBlockfrostToken      :: Maybe BlockfrostToken
    , envDelegationFolder     :: FilePath
    , envFrequency            :: Int
    -- ^ Frequency of search for new delegations in seconds
    , envMaxDelay             :: Int
    -- ^ Maximum permissible synchronization delay in seconds, if exceeded, an error will be thrown
    , envMinTokenNumber       :: Integer
    -- ^ The number of tokens, exceeding which the server gets into the current servers endpoint
    , envRewardTokenThreshold :: Integer
    -- ^ The number of tokens that limits the distribution of rewards
    , envCurrencySymbol       :: CurrencySymbol
    , envTokenName            :: TokenName
    , envCheckSig             :: Bool
    -- ^ There is no signature checks in tests untill cardano-wallet signature fix
    -- https://github.com/cardano-foundation/cardano-wallet/issues/4104
    -- (They are still present outside of tests)
    , envProgress             :: IORef (Progress, Time.UTCTime)
    -- ^ Last delegation progress with it's last update time
    , envTokenBalance         :: IORef (Map PubKeyHash Integer, Time.UTCTime)
    -- ^ Last token balance with it's last update time
    }

loadDelegationEnv :: MonadIO m => DelegationConfig -> m DelegationEnv
loadDelegationEnv DelegationConfig{..} = liftIO $ do
    envProgress <- initProgress cDelegationFolder >>= newIORef
    envTokenBalance <- newIORef (mempty, Time.UTCTime (toEnum 0) 0)
    envBlockfrostToken <- decodeOrErrorFromFile $ fromMaybe "blockfrost.token" cMaestroTokenFilePath
    envMaestroToken <- decodeOrErrorFromFile $ fromMaybe "maestro.token" cMaestroTokenFilePath
    pure DelegationEnv
        { envNetworkId            = cNetworkId
        , envDelegationFolder     = cDelegationFolder
        , envFrequency            = cFrequency
        , envMaxDelay             = cMaxDelay
        , envMinTokenNumber       = cMinTokenNumber
        , envRewardTokenThreshold = cRewardTokenThreshold
        , envCurrencySymbol       = cDelegationCurrencySymbol
        , envTokenName            = cDelegationTokenName
        , envCheckSig             = True
        , ..
        }

setTokenBalance :: MonadIO m => AuxillaryEnvOf api ~ DelegationEnv => Map PubKeyHash Integer -> Time.UTCTime -> AppT api m ()
setTokenBalance b t = (liftIO . flip atomicWriteIORef (b, t)) . envTokenBalance =<< getAuxillaryEnv

getBalances :: MonadMaestro (AppT api m) => CurrencySymbol -> TokenName -> AppT api m (Map PubKeyHash Integer)
getBalances = Maestro.getAccountAddressesHoldingAssets

------------------------------------------------------------------ Helpers ------------------------------------------------------------------

data Progress = Progress
    { pLastTxId   :: Maybe TxId
    , pDelgations :: [Delegation]
    } deriving (Show, Generic, FromJSON, ToJSON)

setProgress :: MonadIO m => AuxillaryEnvOf api ~ DelegationEnv => Progress -> Time.UTCTime -> AppT api m ()
setProgress p t = (liftIO . flip atomicWriteIORef (p, t)) . envProgress =<< getAuxillaryEnv

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

findDeleg ::
    ( Monad m
    , AuxillaryEnvOf api ~ DelegationEnv
    , MonadBlockfrost (AppT api m)
    , MonadMaestro (AppT api m)
    ) => TxId -> AppT api m (Maybe Delegation)
findDeleg txId = runMaybeT $ do
    DelegationEnv{..} <- lift getAuxillaryEnv
    TxDetailsResponse{..} <- MaybeT $ Maestro.getTxDetails txId
    MaybeT $ fmap (listToMaybe . catMaybes) $ forM tdrOutputs $ \TxDetailsOutput{..} -> runMaybeT $ do
        stakeKey  <- hoistMaybe $ getStakeKey tdoAddress
        (dh, dfq) <- hoistMaybe tdoDatum
        Datum dat <- case dfq of
            DatumUnknown   -> MaybeT $ Bf.getDatumByHash dh
            DatumInline da -> pure da
            DatumInBody da -> pure da
        ["ENCOINS", "Delegate", skBbs, ipBbs] <- hoistMaybe $ fromBuiltinData dat
        let ipAddr = fromBuiltin $ decodeUtf8 ipBbs
        when envCheckSig $ guard $ PubKeyHash skBbs `elem` tdrAdditionalSigners && isValidIp ipAddr
        pure $ Delegation (addressCredential tdoAddress) stakeKey (TxOutRef tdoTxHash tdoIndex) tdrSlot ipAddr
    where
        hoistMaybe = MaybeT . pure

isValidIp :: Text -> Bool
isValidIp txt = or $ [isSimpleURI, isURI, isIPv4address] <&> ($ T.unpack txt)
    where
        isSimpleURI  = isURI . ("http://" <>)

data RelayAddress = RelayAddress
    { raAddress  :: Text
    , raPort     :: Maybe Int
    } deriving (Show, Ord)

instance Eq RelayAddress where
    r1 == r2 = eqAddresses && eqPorts
        where
            eqAddresses = raAddress r1 == raAddress r2
            eqPorts     = any isNothing (raPort <$> [r1, r2]) || raPort r1 == raPort r2

instance FromJSON RelayAddress where
    parseJSON = fmap toRelayAddress . parseJSON

instance FromJSONKey RelayAddress where
    fromJSONKey = FromJSONKeyText toRelayAddress

instance ToJSON RelayAddress where
    toJSON = toJSON . fromRelayAddress

instance ToJSONKey RelayAddress where
    toJSONKey = toJSONKeyText fromRelayAddress

toRelayAddress :: Text -> RelayAddress
toRelayAddress addr =
        let addr' = trimProtocol $ trimEndSlash addr
            (raAddress, raPort)  = splitPort addr'
        in RelayAddress{..}
    where
        trimEndSlash txt
            | T.last txt == '/' && T.length txt > 1 = T.init txt
            | otherwise = txt
        trimProtocol txt = fromMaybe txt $ T.stripPrefix "http://" txt <|> T.stripPrefix "https://" txt
        splitPort txt = let (txt', mbPort) = readMaybe . T.unpack <$> T.breakOnEnd ":" txt in
            if isJust mbPort then (T.init txt', mbPort) else (txt, mbPort)

fromRelayAddress :: RelayAddress -> Text
fromRelayAddress RelayAddress{..} = "http://" <> raAddress <> maybe "" ((":" <>) . T.pack . show) raPort

-- Remove end slash, protocol prefix and port from URL address. Doesn't remove port from localhost
trimIp :: Text -> Text
trimIp txt
    | raAddress == "localhost" = "localhost" <> maybe "" ((":" <>) . T.pack . show) raPort
    | otherwise = raAddress
    where
        RelayAddress{..} = toRelayAddress txt

-- Make map with ips and sum of delegated tokens from list with each delegation ip and token amount
concatIpsWithBalances :: [(Text, Integer)] -> Map Text Integer
concatIpsWithBalances = Map.fromList
                      . map (\xs -> (fst $ NonEmpty.head xs, sum $ snd <$> xs))
                      . NonEmpty.groupBy ((==) `on` trimIp . fst)
                      . sortBy (compare `on` trimIp . fst)

data Delegation = Delegation
    { delegCredential :: Credential
    -- ^ Credential of address which makes delegation tx
    , delegStakeKey   :: PubKeyHash
    -- ^ Stake key of address which makes delegation tx
    , delegTxOutRef   :: TxOutRef
    -- ^ TxOutRef of delegation tx
    , delegCreated    :: Slot
    -- ^ Slot of delegation tx
    , delegIp         :: Text
    -- ^ Address of delegated server, possibly specified with port, protocol and backslash
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

delegAddress :: Delegation -> Address
delegAddress d = Address (delegCredential d) (Just $ StakingHash $ PubKeyCredential $ delegStakeKey d)

lastDelegation :: [Delegation] -> Maybe Delegation
lastDelegation = listToMaybe . sortBy (compare `on` Down . delegCreated)

removeDuplicates :: [Delegation] -> [Delegation]
removeDuplicates = fmap (NonEmpty.head . NonEmpty.sortBy (compare `on` Down . delegCreated))
                 . NonEmpty.groupBy ((==) `on` delegStakeKey)
                 . sortBy (compare `on` delegStakeKey)