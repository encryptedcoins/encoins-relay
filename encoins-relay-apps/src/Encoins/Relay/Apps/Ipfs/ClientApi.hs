{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}


module Encoins.Relay.Apps.Ipfs.ClientApi where

import           Encoins.Relay.Apps.Ipfs.Types

import           Data.Aeson                    (FromJSON (..),
                                                Options (fieldLabelModifier),
                                                ToJSON (..), camelTo2,
                                                defaultOptions,
                                                genericParseJSON, withObject,
                                                (.:), (.:?))
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import           GHC.Generics                  (Generic)
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
