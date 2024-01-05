{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Apps.Ipfs.Types where

import           Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import           Data.Aeson           (FromJSON (..),
                                       Options (fieldLabelModifier),
                                       ToJSON (..), camelTo2, defaultOptions,
                                       genericParseJSON, withObject, (.:),
                                       (.:?))
import           Data.Text            (Text)
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.HTTP.Client  (Manager)
import           Servant.Client       (BaseUrl)


data IpfsEnv = MkIpfsEnv
  { envPinataPinUrl   :: BaseUrl
  , envPinataFetchUrl :: BaseUrl
  , envAuthKey        :: Text
  , envManager        :: Manager
  }

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
  { name      :: Maybe Text
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
