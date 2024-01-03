{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}


module Encoins.Relay.Apps.Ipfs.ClientApi where

import           Data.Aeson     (FromJSON (..), Options (fieldLabelModifier),
                                 ToJSON (..), camelTo2, defaultOptions,
                                 genericParseJSON, withObject, (.:), (.:?))
import           Data.Proxy     (Proxy (..))
import           Data.Text      (Text)
import           Data.Time      (UTCTime)
import           GHC.Generics   (Generic)
import           Servant.API
import           Servant.Client



type IpfsAPI =
       "pinning" :> "pinJSONToIPFS"
                 :> Auth
                 :> ReqBody '[JSON] Token
                 :> Post '[JSON] PinJsonResponse
  :<|> "ipfs" :> Capture "cip" Text :> Get '[JSON] TokenKey
  :<|> "data" :> Auth :> "pinList" :> Get '[JSON] Files
  :<|> "pinning" :> "unpin"
                 :> Auth :> Capture "cip" Text :> Delete '[PlainText] Text
  :<|> "data" :> "pinList"
              :> Auth :> QueryParam "status" Text :> Get '[JSON] Files
  :<|> "data" :> "pinList"
              :> Auth
              :> QueryParam "status" Text
              :> QueryParam "metadata[name]" Text
              :> Get '[JSON] Files

type Auth = Header "Authorization" Text

ipfsApi :: Proxy IpfsAPI
ipfsApi = Proxy

pinJson           :: Maybe Text -> Token -> ClientM PinJsonResponse
fetchByCip        :: Text -> ClientM TokenKey
fetchMetaAll      :: Maybe Text -> ClientM Files
unpinByCip        :: Maybe Text -> Text -> ClientM Text
fetchMetaByStatus :: Maybe Text -> Maybe Text -> ClientM Files
fetchMetaByStatusAndName :: Maybe Text -> Maybe Text -> Maybe Text -> ClientM Files
pinJson :<|> fetchByCip :<|> fetchMetaAll :<|> unpinByCip :<|>
  fetchMetaByStatus :<|> fetchMetaByStatusAndName = client ipfsApi

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

{-
curl --request POST \
     --url https://api.pinata.cloud/pinning/pinJSONToIPFS \
     --header 'accept: application/json' \
     --header 'content-type: application/json' \
     --data '
{
  "pinataContent": {
    "keyHello": "valueWorld",
    "keyHola": "valueMundo"
  },
  "pinataMetadata": {
    "keyvalues": {
      "optionKey": "optionValue"
    },
    "name": "token"
  }
}
'
-}
