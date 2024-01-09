{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Apps.Ipfs.Types where

import           Cardano.Api           (NetworkId)
import           Cardano.Server.Config (HyperTextProtocol (..))
import           Control.Monad.Reader  (ReaderT (..))
import           Data.Aeson            (FromJSON (..),
                                        Options (fieldLabelModifier),
                                        ToJSON (..), camelTo2, defaultOptions,
                                        genericParseJSON, withObject, (.:),
                                        (.:?))
import           Data.Aeson.Casing     (aesonPrefix, snakeCase)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)
import           Network.HTTP.Client   (Manager)
import           Plutus.V1.Ledger.Api  (CurrencySymbol)
import           Servant.Client        (BaseUrl (..), Scheme (..))

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

type IpfsMonad = ReaderT IpfsEnv IO

-- Request / Response types

data TokenStatus = Minted | Burned
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TokenKey = MkTokenKey { tokenKey :: Text }
  deriving newtype (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data MetaOptions = MkMetaOptions
  { status :: TokenStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metadata = MkMetadata
  { name      :: Maybe Text -- Asset Name
  , keyvalues :: Maybe MetaOptions
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Token = Token
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
  , metadata      :: Metadata
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
