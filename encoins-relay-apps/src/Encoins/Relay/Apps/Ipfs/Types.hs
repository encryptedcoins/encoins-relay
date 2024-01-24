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
import           Control.Monad.Reader   (MonadReader, ReaderT (..))
import           Data.Aeson             (FromJSON (..),
                                         Options (fieldLabelModifier),
                                         ToJSON (..), camelTo2, defaultOptions,
                                         genericParseJSON, genericToJSON,
                                         withObject, (.:), (.:?))
import           Data.Aeson.Casing      (aesonPrefix, snakeCase)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime)
import           Data.Time.Clock.POSIX  (POSIXTime)
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (Manager)
import           Plutus.V1.Ledger.Api   (CurrencySymbol)
import           Servant.Client         (BaseUrl (..), Scheme (..))

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
  }

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
  }
  deriving stock (Eq,Show, Generic)

instance FromJSON IpfsConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

mkIpfsEnv :: Manager -> Text -> IpfsConfig -> IpfsEnv
mkIpfsEnv manager pinataToken ipfsConfig = MkIpfsEnv
  { envHyperTextProtocol  = icHyperTextProtocol ipfsConfig
  , envHost               = icHost ipfsConfig
  , envPort               = icPort ipfsConfig
  , envNetworkId          = icNetworkId ipfsConfig
  , envIpfsCurrencySymbol = icIpfsCurrencySymbol ipfsConfig
  , envPinataFetchHost    = mkUrl $ icPinataFetchHost ipfsConfig
  , envPinataPinHost      = mkUrl $ icPinataPinHost ipfsConfig
  , envScheduleDirectory  = icScheduleDirectory ipfsConfig
  , envPinataAuthToken    = mkBearer pinataToken
  , envManager            = manager
  }
  where
    mkBearer :: Text -> Text
    mkBearer jwtToken = "Bearer " <> jwtToken
    mkUrl :: Text -> BaseUrl
    mkUrl host = BaseUrl Https (T.unpack host) 443 ""

-- type IpfsMonad = ReaderT IpfsEnv IO
newtype IpfsMonad a = IpfsMonad {unIpfsMonad :: ReaderT IpfsEnv IO a}
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

-- IPFS Request/Response types

-- data TokenStatus = Minted | Burned
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (ToJSON, FromJSON)

newtype TokenKey = MkTokenKey { tokenKey :: Text }
  deriving newtype (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  , reqSecretKey :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CloudRequest where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

mkTokentoIpfs :: Text -> CloudRequest -> TokenToIpfs
mkTokentoIpfs clientId req = MkTokenToIpfs
  { pinataContent = MkTokenKey $ reqSecretKey req
  , pinataMetadata = MkMetadata
      (reqAssetName req)
      (MkMetaOptions clientId)
  }

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
