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
  :<|> "ipfs" :> Capture "cip" Cip :> Get '[JSON] EncryptedToken
  :<|> "data" :> Auth :> "pinList" :> Get '[JSON] Files
  :<|> "pinning" :> "unpin"
                 :> Auth :> Capture "cip" Cip :> Delete '[PlainText] Text
  :<|> "data" :> "pinList"
              :> Auth :> QueryParam "status" Text :> Get '[JSON] Files
  :<|> "data" :> "pinList"
              :> Auth
              :> QueryParam "status" Text
              :> QueryParam "metadata[name]" AssetName
              :> Get '[JSON] Files
  :<|> "data" :> "pinList"
              :> Auth
              :> QueryParam "status" Text
              :> QueryParam "metadata[keyvalues][client_id]" Text
              :> Get '[JSON] Files
  :<|> "data" :> "pinList"
              :> Auth
              :> QueryParam "status" Text
              :> QueryParam "metadata[name]" AssetName
              :> QueryParam "metadata[keyvalues][client_id]" Text
              :> Get '[JSON] Files
  :<|> "data" :> "testAuthentication"
              :> Auth
              :> Get '[JSON] CheckTokenResponse

type Auth = Header "Authorization" Text

clientIpfsApi :: Proxy ClientIpfsAPI
clientIpfsApi = Proxy

pinJson                   :: Maybe Text -> TokenToIpfs -> ClientM PinJsonResponse
fetchByCip                :: Cip -> ClientM EncryptedToken
fetchMetaAll              :: Maybe Text -> ClientM Files
unpinByCip                :: Maybe Text -> Cip -> ClientM Text
fetchByStatus             :: Maybe Text -> Maybe Text -> ClientM Files
fetchByStatusName         :: Maybe Text -> Maybe Text -> Maybe AssetName -> ClientM Files
fetchByStatusKeyvalue     :: Maybe Text -> Maybe Text -> Maybe Text -> ClientM Files
fetchByStatusNameKeyvalue :: Maybe Text
  -> Maybe Text
  -> Maybe AssetName
  -> Maybe Text
  -> ClientM Files
testAuthentication        :: Maybe Text -> ClientM CheckTokenResponse
pinJson :<|> fetchByCip :<|> fetchMetaAll :<|> unpinByCip :<|>
  fetchByStatus :<|> fetchByStatusName :<|> fetchByStatusKeyvalue
  :<|> fetchByStatusNameKeyvalue :<|> testAuthentication = client clientIpfsApi
