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

import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import           Servant.API
import           Servant.Client

type ClientIpfsAPI =
       "pinning" :> "pinJSONToIPFS"
                 :> Auth
                 :> ReqBody '[JSON] TokenToIpfs
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
  :<|> "data" :> "pinList"
              :> Auth
              :> QueryParam "status" Text
              :> QueryParam "metadata[keyvalue][client_id]" Text
              :> Get '[JSON] Files

type Auth = Header "Authorization" Text

clientIpfsApi :: Proxy ClientIpfsAPI
clientIpfsApi = Proxy

pinJson               :: Maybe Text -> TokenToIpfs -> ClientM PinJsonResponse
fetchByCip            :: Text -> ClientM TokenKey
fetchMetaAll          :: Maybe Text -> ClientM Files
unpinByCip            :: Maybe Text -> Text -> ClientM Text
fetchByStatus         :: Maybe Text -> Maybe Text -> ClientM Files
fetchByStatusName     :: Maybe Text -> Maybe Text -> Maybe Text -> ClientM Files
fetchByStatusKeyvalue :: Maybe Text -> Maybe Text -> Maybe Text -> ClientM Files
pinJson :<|> fetchByCip :<|> fetchMetaAll :<|> unpinByCip :<|>
  fetchByStatus :<|> fetchByStatusName :<|> fetchByStatusKeyvalue = client clientIpfsApi
