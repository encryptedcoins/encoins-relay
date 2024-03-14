{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Apps.Save.Types where

import           Encoins.Common.Transform      (toJsonText)

import           Cardano.Api                   (NetworkId)
import           Cardano.Server.Config         (HyperTextProtocol (..))
import           Control.Exception.Safe        (Exception, MonadCatch,
                                                MonadThrow)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, ReaderT (..), asks,
                                                local)
import           Data.Aeson                    (FromJSON (..), FromJSONKey,
                                                Options (fieldLabelModifier),
                                                SumEncoding (..), ToJSON (..),
                                                ToJSONKey, camelTo2,
                                                constructorTagModifier,
                                                defaultOptions,
                                                genericParseJSON, genericToJSON,
                                                sumEncoding,
                                                tagSingleConstructors,
                                                withObject, (.:), (.:?))
import           Data.Aeson.Casing             (aesonPrefix, camelCase,
                                                snakeCase)
import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import           Data.Time.Clock.POSIX         (POSIXTime)
import           GHC.Generics                  (Generic)
import           Katip
import           Network.HTTP.Client           (Manager)
import           Numeric.Natural               (Natural)
import           Plutus.V1.Ledger.Api          (CurrencySymbol)
import           PlutusAppsExtra.Api.Maestro   (MaestroToken, MonadMaestro (..))
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           Servant.API                   (ToHttpApiData)
import           Servant.Client                (ClientError)
import           Data.Semigroup                 (Max (..))

-- General types

data SaveEnv = MkIpfsEnv
  { envHyperTextProtocol :: HyperTextProtocol
  , envHost              :: Text
  , envPort              :: Int
  , envNetworkId         :: NetworkId
  , envMaestroToken      :: MaestroToken
  , envCurrencySymbol    :: CurrencySymbol
  , envSaveDirectory     :: FilePath
  , envManager           :: Manager
  , envLogEnv            :: LogEnv
  , envKContext          :: LogContexts
  , envKNamespace        :: Namespace
  , envFormatMessage     :: Bool -- Pretty print message or not
  }

-- Format of severity in json file:
-- debug, info, notice, warning, error, critical, alert, emergency
-- Format of verbosity in json file: V0, V1, V2, V3
data SaveConfig = MkSaveConfig
  {
    icHyperTextProtocol    :: HyperTextProtocol
  , icHost                 :: Text
  , icPort                 :: Int
  , icNetworkId            :: NetworkId
  , icMaestroTokenFilePath :: FilePath
  , icCurrencySymbol       :: CurrencySymbol
  , icSaveDirectory        :: FilePath
  , icEnvironment          :: Environment
  , icVerbosity            :: Verbosity
  , icSeverity             :: Severity
  , icFormatMessage        :: Bool
  }
  deriving stock (Eq,Show, Generic)

instance FromJSON SaveConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype SaveMonad a = MkSaveMonad {getSaveMonad :: ReaderT SaveEnv IO a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader SaveEnv
        )

runSaveMonad :: SaveEnv -> SaveMonad a -> IO a
runSaveMonad env = (`runReaderT` env) . getSaveMonad

instance HasNetworkId SaveMonad where
  getNetworkId = asks envNetworkId

instance MonadMaestro SaveMonad where
  getMaestroToken = asks envMaestroToken

instance Katip SaveMonad where
  getLogEnv = asks envLogEnv
  localLogEnv f (MkSaveMonad m) =
    MkSaveMonad (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance KatipContext SaveMonad where
  getKatipContext = asks envKContext
  localKatipContext f (MkSaveMonad m) =
    MkSaveMonad (local (\s -> s {envKContext = f (envKContext s)}) m)
  getKatipNamespace = asks envKNamespace
  localKatipNamespace f (MkSaveMonad m) =
    MkSaveMonad (local (\s -> s {envKNamespace = f (envKNamespace s)}) m)

newtype EncryptedToken= MkEncryptedToken { getEncryptedToken :: Text }
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance FromJSON EncryptedToken where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON EncryptedToken where
   toJSON = genericToJSON $ aesonPrefix snakeCase

-- EncryptedToken and EncryptedSecret are semantically the same.
-- The difference is in their JSON instances.
-- EncryptedSecret is encoded as Text, whereas
-- EncryptedToken is encoded as Object with field name 'token_key'
-- We don't use EncryptedToken-like encoding for EncryptedSecret
-- because it increase the size of request needlessly.
newtype EncryptedSecret = MkEncryptedSecret { getEncryptedSecret :: Text }
  deriving newtype (Eq, Show, ToJSON, FromJSON)
  deriving stock (Generic)

data MetaOptions = MkMetaOptions
  { moClientId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MetaOptions where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON MetaOptions where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data MetadataLoose = MkMetadataLoose
  { mrName      :: Maybe AssetName
  , mrKeyvalues :: Maybe MetaOptions
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MetadataLoose where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PinataValue = Pvs Text | Pvi Integer | Pvd Double | Pvt UTCTime
  deriving stock (Show, Eq, Generic)

instance FromJSON PinataValue where
   parseJSON = genericParseJSON $ defaultOptions{sumEncoding = UntaggedValue}

instance ToJSON PinataValue where
   toJSON = genericToJSON $ defaultOptions{sumEncoding = UntaggedValue}

data PinataOption
  = PoGt -- (greater than)
  | PoGte -- (greater than or equal)
  | PoLt -- (less than)
  | PoLte -- (less than or equal)
  | PoNe -- (not equal to)
  | PoEq -- (equal to)
  | PoBetween
  | PoNotBetween
  | PoLike
  deriving stock (Show, Eq, Generic)

instance FromJSON PinataOption where
   parseJSON = genericParseJSON $
    defaultOptions{constructorTagModifier = camelCase . drop 2}

instance ToJSON PinataOption where
   toJSON = genericToJSON $
    defaultOptions{constructorTagModifier = camelCase . drop 2}

data KeyValue = MkKeyValue
  { kvValue :: PinataValue
  , kvOp    :: PinataOption
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON KeyValue where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON KeyValue where
   toJSON = genericToJSON $ aesonPrefix snakeCase

mkKeyvalueClientId :: AesKeyHash -> Text
mkKeyvalueClientId (MkAesKeyHash key) =
  let keyValue = MkKeyValue (Pvs key) PoEq
  in toJsonText keyValue

-- Request body from backend to IPFS
data TokenToIpfs = MkTokenToIpfs
  { pinataContent  :: EncryptedToken
  , pinataMetadata :: Metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkTokentoIpfs :: AesKeyHash -> SaveRequest -> TokenToIpfs
mkTokentoIpfs clientId req = MkTokenToIpfs
  { pinataContent = MkEncryptedToken $ getEncryptedSecret $ ppSecretKey req
  , pinataMetadata = MkMetadata
      (ppAssetName req)
      (MkMetaOptions $ getAesKeyHash clientId)
  }

data PinJsonResponse = MkPinJsonResponse
  { ipfsHash    :: Cip
  , pinSize     :: Int
  , timestamp   :: UTCTime
  , isDuplicate :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PinJsonResponse where
    parseJSON = withObject "PinJsonResponse" $ \o -> do
        ipfsHash    <- o .: "IpfsHash"
        pinSize     <- o .: "PinSize"
        timestamp   <- o .: "Timestamp"
        isDuplicate <- o .:? "isDuplicate"
        pure MkPinJsonResponse{..}

data File = MkFile
  { fileId        :: Text
  , ipfsPinHash   :: Cip
  , size          :: Int
  , userId        :: Text
  , datePinned    :: Maybe UTCTime
  , dateUnpinned  :: Maybe UTCTime
  , metadata      :: MetadataLoose
  , regions       :: [Regions]
  , mimeType      :: Text
  , numberOfFiles :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON File where
    parseJSON = genericParseJSON
      defaultOptions{ fieldLabelModifier = modifyField }

modifyField :: String -> String
modifyField "fileId" = "id"
modifyField key      = camelTo2 '_' key

data Regions = MkRegions
  { regionId                :: Text
  , currentReplicationCount :: Int
  , desiredReplicationCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Files = MkFiles
  { count :: Int
  , rows  :: [File]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- Backend Request/Response types

newtype AssetName = MkAssetName { getAssetName :: Text }
  deriving newtype (Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, ToHttpApiData)
  deriving stock (Generic)

data Metadata = MkMetadata
  { mName      :: AssetName
  , mKeyvalues :: MetaOptions
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Metadata where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Metadata where
   toJSON = genericToJSON $ aesonPrefix snakeCase

-- Request body from frontend to backend
data SaveRequest = MkSaveRequest
  { ppAssetName :: AssetName
  , ppSecretKey :: EncryptedSecret
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SaveRequest where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- Used to tag ipfs status
data IpfsResponse = IpfsPinned | IpfsUnpinned | IpfsFail Text
  deriving stock (Eq, Show)

-- Used to tag maestro status
data CoinStatus = CoinMinted | CoinBurned | CoinDiscarded Text | CoinError Text
  deriving stock (Eq, Show)

-- Used to tag save status
data FilterAssetStatus = Exist | FMax (Max Natural)
  deriving stock (Eq, Show)

instance Semigroup FilterAssetStatus where
  Exist <> _ = Exist
  _ <> Exist = Exist
  FMax n <> FMax m = FMax (n <> m)

-- Used to response to the client
data SaveStatus = Saved | SaveError | Discarded
  deriving stock (Show, Eq, Generic)

instance ToJSON SaveStatus where
  toJSON = genericToJSON $
    defaultOptions{tagSingleConstructors = True}

data StatusResponse = MkStatusResponse
  { spStatusResponse :: SaveStatus
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON StatusResponse where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data SaveToken = MkSaveToken
  { stAssetName :: AssetName
  , stSecret    :: EncryptedSecret
  , stDuplicate :: Natural
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SaveToken where
   toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SaveToken where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data RottenToken = MkRottenToken
  { rtAssetName  :: AssetName
  , rtRemoveTime :: POSIXTime
  , rtCip        :: Cip
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RottenToken where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON RottenToken where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data RestoreResponse = MkRestoreResponse
  { rrAssetName       :: AssetName
  , rrEncryptedSecret :: EncryptedSecret
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RestoreResponse where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data RestoreError = Client ClientError | InvalidStatus CoinStatus
  deriving stock (Show, Eq)

-- Hash of aes key to save it in metadata field as clientId on IPFS
-- For identifying which token to fetch
newtype AesKeyHash = MkAesKeyHash { getAesKeyHash :: Text }
  deriving newtype (Eq, Show, ToJSON, FromJSON, ToHttpApiData)
  deriving stock (Generic)

newtype Cip = MkCip { getCip :: Text }
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON, ToHttpApiData)
  deriving stock (Generic)

data CheckTokenResponse = MkCheckTokenResponse
  {
    vtMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CheckTokenResponse where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data IPFSException = InvalidPinataToken
    deriving Show

instance Exception IPFSException
