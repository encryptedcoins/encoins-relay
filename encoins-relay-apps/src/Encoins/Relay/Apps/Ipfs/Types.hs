{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Apps.Ipfs.Types where

import           Cardano.Api            (NetworkId)
import           Cardano.Server.Config  (HyperTextProtocol (..))
import           Control.Exception.Safe (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), local, asks)
import           Data.Aeson             (FromJSON (..),
                                         Options (fieldLabelModifier),
                                         ToJSON (..), camelTo2, defaultOptions,
                                         genericParseJSON, genericToJSON,
                                         withObject, (.:), (.:?))
import           Data.Aeson.Casing      (aesonPrefix, snakeCase)
import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Data.Time.Clock.POSIX  (POSIXTime)
import           GHC.Generics           (Generic)
import           Katip
import           Network.HTTP.Client    (Manager)
import           Plutus.V1.Ledger.Api   (CurrencySymbol)
import           Servant.Client         (BaseUrl (..), ClientError)

-- General types

data IpfsEnv = MkIpfsEnv
  { envHyperTextProtocol  :: HyperTextProtocol
  , envHost               :: Text
  , envPort               :: Int
  , envNetworkId          :: NetworkId
  , envIpfsCurrencySymbol :: CurrencySymbol
  , envPinataFetchHost    :: BaseUrl
  , envPinataPinHost      :: BaseUrl
  , envScheduleDirectory  :: FilePath
  , envPinataAuthToken    :: Text
  , envManager            :: Manager
  , envLogEnv             :: LogEnv
  , envKContext           :: LogContexts
  , envKNamespace         :: Namespace
  , envFormatMessage      :: Bool -- Pretty print message
  }

-- Format of severity in json file:
-- debug, info, notice, warning, error, critical, alert, emergency
-- Format of verbosity in json file: V0, V1, V2, V3
data IpfsConfig = MkIpfsConfig
  {
    icHyperTextProtocol  :: HyperTextProtocol
  , icHost               :: Text
  , icPort               :: Int
  , icNetworkId          :: NetworkId
  , icIpfsCurrencySymbol :: CurrencySymbol
  , icPinataFetchHost    :: Text
  , icPinataPinHost      :: Text
  , icScheduleDirectory  :: FilePath
  , icEnvironment        :: Environment
  , icVerbosity          :: Verbosity
  , icSeverity           :: Severity
  , icFormatMessage      :: Bool
  }
  deriving stock (Eq,Show, Generic)

instance FromJSON IpfsConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype IpfsMonad a = MkIpfsMonad {unIpfsMonad :: ReaderT IpfsEnv IO a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader IpfsEnv
        )

runIpfsMonad :: IpfsEnv -> IpfsMonad a -> IO a
runIpfsMonad env = (`runReaderT` env) . unIpfsMonad

instance Katip IpfsMonad where
  getLogEnv = asks envLogEnv
  localLogEnv f (MkIpfsMonad m) =
    MkIpfsMonad (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance KatipContext IpfsMonad where
  getKatipContext = asks envKContext
  localKatipContext f (MkIpfsMonad m) =
    MkIpfsMonad (local (\s -> s {envKContext = f (envKContext s)}) m)
  getKatipNamespace = asks envKNamespace
  localKatipNamespace f (MkIpfsMonad m) =
    MkIpfsMonad (local (\s -> s {envKNamespace = f (envKNamespace s)}) m)

newtype TokenKey = MkTokenKey { tkTokenKey :: Text }
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance FromJSON TokenKey where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON TokenKey where
   toJSON = genericToJSON $ aesonPrefix snakeCase

-- TokenKey and EncryptedSecret are semantically the same.
-- The difference is in their JSON instances.
-- EncryptedSecret is encoded as Text, whereas
-- TokenKey is encoded as Object with field name 'token_key'
-- We don't use TokenKey-like encoding for EncryptedSecret
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
  { mrName      :: Maybe Text -- Asset Name
  , mrKeyvalues :: Maybe MetaOptions
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MetadataLoose where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- Request body from backend to IPFS
data TokenToIpfs = MkTokenToIpfs
  { pinataContent  :: TokenKey
  , pinataMetadata :: Metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkTokentoIpfs :: Text -> CloudRequest -> TokenToIpfs
mkTokentoIpfs clientId req = MkTokenToIpfs
  { pinataContent = MkTokenKey $ getEncryptedSecret $ reqSecretKey req
  , pinataMetadata = MkMetadata
      (reqAssetName req)
      (MkMetaOptions clientId)
  }

data PinJsonResponse = MkPinJsonResponse
  { ipfsHash    :: Text
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
  , ipfsPinHash   :: Text
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

data Metadata = MkMetadata
  { mName      :: Text -- Asset Name
  , mKeyvalues :: MetaOptions
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Metadata where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Metadata where
   toJSON = genericToJSON $ aesonPrefix snakeCase

-- Request body from frontend to backend
data CloudRequest = MkCloudRequest
  { reqAssetName :: Text
  , reqSecretKey :: EncryptedSecret
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CloudRequest where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data IpfsStatus = Pinned | Unpinned | FileError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data CoinStatus = Minted | Burned | CoinError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data CloudResponse = MkCloudResponse
  { resIpfsStatus :: Maybe IpfsStatus
  , resCoinStatus :: Maybe CoinStatus
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CloudResponse where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data RottenToken = MkRottenToken
  { rtAssetName  :: Text
  , rtRemoveTime :: POSIXTime
  , rtCip        :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RottenToken where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON RottenToken where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data RestoreResponse = MkRestoreResponse
  { rrAssetName :: Text
  , rrSecretKey :: TokenKey
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RestoreResponse where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data RestoreError = Client ClientError | InvalidStatus CoinStatus
  deriving stock (Show, Eq)
