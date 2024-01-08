{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Encoins.Relay.Apps.Delegation.Internal where

import           Cardano.Api                   (NetworkId)
import           Cardano.Server.Config         (CardanoServerConfig (..), HyperTextProtocol (..))
import           Cardano.Server.Utils.Logger   (HasLogger (..), Logger)
import           Control.Applicative           ((<|>))
import           Control.Exception             (throw)
import           Control.Monad                 (forM, guard, when)
import           Control.Monad.Catch           (MonadCatch, MonadThrow (..))
import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (MonadReader (ask), ReaderT (..), asks)
import           Control.Monad.Trans.Maybe     (MaybeT (..))
import           Data.Aeson                    (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                                                ToJSONKey (..), genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Aeson.Types              (toJSONKeyText)
import           Data.Function                 (on)
import           Data.Functor                  ((<&>))
import           Data.IORef                    (IORef, atomicWriteIORef)
import           Data.List                     (sortBy)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import           Data.Ord                      (Down (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import           GHC.Generics                  (Generic)
import           Ledger                        (Address (..), Credential, Datum (..), DatumFromQuery (..), PubKeyHash (..), Slot,
                                                TxId (..), TxOutRef (..))
import           Network.URI                   (isIPv4address, isURI)
import           Plutus.V1.Ledger.Api          (Credential (..), CurrencySymbol, FromData (..), StakingCredential (..), TokenName,
                                                fromBuiltin)
import qualified PlutusAppsExtra.IO.Blockfrost as Bf
import qualified PlutusAppsExtra.IO.Maestro    as Maestro
import           PlutusAppsExtra.Utils.Address (getStakeKey)
import           PlutusAppsExtra.Utils.Maestro (TxDetailsOutput (..), TxDetailsResponse (..))
import           PlutusTx.Builtins             (decodeUtf8)
import           Servant                       (Handler, ServerError, runHandler)
import           Text.Read                     (readMaybe)

newtype DelegationM a = DelegationM {unDelegationM :: ReaderT DelegationEnv Servant.Handler a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader DelegationEnv
        , MonadError Servant.ServerError
        )

runDelegationM :: DelegationEnv -> DelegationM a -> IO a
runDelegationM env = fmap (either throw id) . Servant.runHandler . (`runReaderT` env) . unDelegationM

instance HasLogger DelegationM where
    getLogger = asks dEnvLogger
    getLoggerFilePath = asks dEnvLoggerFp

data DelegationEnv = DelegationEnv
    { dEnvLogger               :: Logger DelegationM
    , dEnvLoggerFp             :: Maybe FilePath
    , dEnvNetworkId            :: NetworkId
    , dEnvHost                 :: Text
    , dEnvPort                 :: Int
    , dEnvHyperTextProtocol    :: HyperTextProtocol
    , dEnvDelegationFolder     :: FilePath
    , dEnvFrequency            :: Int
    -- ^ Frequency of search for new delegations in seconds
    , dEnvMaxDelay             :: Int
    -- ^ Maximum permissible synchronization delay in seconds, if exceeded, an error will be thrown
    , dEnvMinTokenNumber       :: Integer
    -- ^ The number of tokens, exceeding which the server gets into the current servers endpoint
    , dEnvRewardTokenThreshold :: Integer
    -- ^ The number of tokens that limits the distribution of rewards
    , dEnvCurrencySymbol       :: CurrencySymbol
    , dEnvTokenName            :: TokenName
    , dEnvCheckSig             :: Bool
    -- ^ There is no signature checks in tests untill cardano-wallet signature fix
    -- https://github.com/cardano-foundation/cardano-wallet/issues/4104
    -- (They are still present outside of tests)
    , dEnvProgress             :: IORef (Progress, Time.UTCTime)
    -- ^ Last delegation progress with it's last update time
    , dEnvTokenBalance         :: IORef (Map PubKeyHash Integer, Time.UTCTime)
    -- ^ Last token balance with it's last update time
    }

setProgress :: Progress -> Time.UTCTime -> DelegationM ()
setProgress p t = asks dEnvProgress >>= (liftIO . flip atomicWriteIORef (p, t))

setTokenBalance :: Map PubKeyHash Integer -> Time.UTCTime -> DelegationM ()
setTokenBalance b t = asks dEnvTokenBalance >>= (liftIO . flip atomicWriteIORef (b, t))

instance CardanoServerConfig DelegationEnv where
    configHost              = dEnvHost
    configPort              = dEnvPort
    configHyperTextProtocol = dEnvHyperTextProtocol


data DelegConfig = DelegConfig
    { cHost                     :: Text
    , cPort                     :: Int
    , cHyperTextProtocol        :: HyperTextProtocol
    , cNetworkId                :: NetworkId
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

instance FromJSON DelegConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance CardanoServerConfig DelegConfig where
    configHost              = cHost
    configPort              = cPort
    configHyperTextProtocol = cHyperTextProtocol

------------------------------------------------------------------ Helpers ------------------------------------------------------------------

data Progress = Progress
    { pLastTxId   :: Maybe TxId
    , pDelgations :: [Delegation]
    } deriving (Show, Generic, FromJSON, ToJSON)

findDeleg :: TxId -> DelegationM (Maybe Delegation)
findDeleg txId = runMaybeT $ do
    DelegationEnv{..} <- ask
    TxDetailsResponse{..} <- MaybeT $ liftIO $ Maestro.getTxDetails dEnvNetworkId txId
    MaybeT $ fmap (listToMaybe . catMaybes) $ forM tdrOutputs $ \TxDetailsOutput{..} -> runMaybeT $ do
        stakeKey  <- hoistMaybe $ getStakeKey tdoAddress
        (dh, dfq) <- hoistMaybe tdoDatum
        Datum dat <- case dfq of
            DatumUnknown   -> MaybeT $ liftIO $ Bf.getDatumByHash dEnvNetworkId dh
            DatumInline da -> pure da
            DatumInBody da -> pure da
        ["ENCOINS", "Delegate", skBbs, ipBbs] <- hoistMaybe $ fromBuiltinData dat
        let ipAddr = fromBuiltin $ decodeUtf8 ipBbs
        when dEnvCheckSig $ guard $ PubKeyHash skBbs `elem` tdrAdditionalSigners && isValidIp ipAddr
        pure $ Delegation (addressCredential tdoAddress) stakeKey (TxOutRef tdoTxHash tdoIndex) tdrSlot ipAddr
    where
        hoistMaybe = MaybeT . pure

getBalances :: MonadIO m => NetworkId -> CurrencySymbol -> TokenName -> m (Map PubKeyHash Integer)
getBalances network cs tokenName = liftIO $ Maestro.getAccountAddressesHoldingAssets network cs tokenName

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

-- Remove end slash, protocol prefix and port from URL address
trimIp :: Text -> Text
trimIp = raAddress . toRelayAddress

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