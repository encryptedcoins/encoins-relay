{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}


module Encoins.Relay.Apps.Ipfs.Client where

import qualified Data.ByteString                   as BS
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as TE
import           Encoins.Relay.Apps.Ipfs.ClientApi
import           Network.HTTP.Client               hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.Client

import Text.Pretty.Simple


ipfsClient :: IO ()
ipfsClient = do
  key <- auth <$> pinataKey "pinata_jwt_token.txt"
  manager <- newManager tlsManagerSettings
  res <- pinJsonRequest manager key token
  pPrint res
  -- pPrint =<< fetchMetaAllRequest manager key
  case res of
    Left err -> pPrint err
    Right r -> do
      let cip = ipfsHash r
      pPrint =<< fetchByCipRequest manager cip
      -- pPrint =<< unpinByCipRequest manager key cip
  pPrint =<< fetchMetaPinnedRequest manager key "pinned"

-- Requests to Pinata API

pinJsonRequest :: Manager -> Text -> Token -> IO (Either ClientError PinJsonResponse)
pinJsonRequest manager authHeader p = do
  runClientM (pinJson (Just authHeader) p) (mkClientEnv manager pinUrl)

fetchByCipRequest :: Manager -> Text -> IO (Either ClientError TokenKey)
fetchByCipRequest manager cip = do
  runClientM (fetchByCip cip) (mkClientEnv manager fetchUrl)

fetchMetaAllRequest :: Manager -> Text -> IO (Either ClientError Files)
fetchMetaAllRequest manager authHeader = do
  runClientM (fetchMetaAll $ Just authHeader) (mkClientEnv manager pinUrl)

unpinByCipRequest :: Manager -> Text -> Text -> IO (Either ClientError Text)
unpinByCipRequest manager authHeader cip = do
  runClientM (unpinByCip (Just authHeader) cip) (mkClientEnv manager pinUrl)

fetchMetaPinnedRequest :: Manager -> Text -> Text -> IO (Either ClientError Files)
fetchMetaPinnedRequest manager authHeader status = do
  runClientM (fetchMetaByStatus (Just authHeader) (Just status)) (mkClientEnv manager pinUrl)

fetchMetaByStatusAndNameRequest :: Manager -> Text -> Text -> Text -> IO (Either ClientError Files)
fetchMetaByStatusAndNameRequest manager authHeader status name = do
  runClientM (fetchMetaByStatusAndName (Just authHeader) (Just status) (Just name)) (mkClientEnv manager pinUrl)

-- Utils

-- Used only to fetch value (files) itself through dedicated gateway
fetchUrl :: BaseUrl
fetchUrl = BaseUrl Https "coral-holy-gibbon-767.mypinata.cloud" 443 ""

-- Used for everything
pinUrl :: BaseUrl
pinUrl = BaseUrl Https "api.pinata.cloud" 443 ""

auth :: Text -> Text
auth jwtToken = "Bearer " <> jwtToken

pinataKey :: FilePath -> IO Text
pinataKey path = TE.decodeUtf8 <$> BS.readFile path

-- TODO: remove after debug
token :: Token
token = Token
  { pinataContent = MkTokenKey "super secret key"
  , pinataMetadata = MkMetadata
      { name = Just "tokenName"
      , keyvalues = Just $ MkMetaOptions Minted
      }
  }