{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}


module Encoins.Relay.Apps.Ipfs.Client where

import           Encoins.Relay.Apps.Ipfs.ClientApi
import           Encoins.Relay.Apps.Ipfs.Config (withIpfsEnv)
import           Encoins.Relay.Apps.Ipfs.Types

import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Reader              (MonadReader (ask))
import           Data.Text                         (Text)
import           Servant.Client

import           Text.Pretty.Simple

ipfsClient :: IO ()
ipfsClient = withIpfsEnv $ \env -> do
  runIpfsMonad env $ do
    res <- pinJsonRequest token
    pPrint res
    -- pPrint =<< fetchMetaAllRequest manager key
    -- case res of
    --   Left err -> pPrint err
    --   Right r -> do
    --     let cip = ipfsHash r
    --     pPrint =<< fetchByCipRequest manager cip
    --     -- pPrint =<< unpinByCipRequest manager key cip
    -- pPrint =<< fetchMetaPinnedRequest manager key "pinned"


-- Requests to Pinata API

-- TODO: Handle ClientError inside of requests

pinJsonRequest :: TokenToIpfs -> IpfsMonad (Either ClientError PinJsonResponse)
pinJsonRequest p = do
  env <- ask
  liftIO $ runClientM
    (pinJson (Just $ envPinataAuthToken env) p)
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchByCipRequest :: Cip -> IpfsMonad (Either ClientError EncryptedToken)
fetchByCipRequest cip = do
  env <- ask
  liftIO $ runClientM
    (fetchByCip cip)
    (mkClientEnv (envManager env) (envPinataFetchHost env))

fetchMetaAllRequest :: IpfsMonad (Either ClientError Files)
fetchMetaAllRequest = do
  env <- ask
  liftIO $ runClientM
    (fetchMetaAll $ Just $ envPinataAuthToken env)
    (mkClientEnv (envManager env) (envPinataPinHost env))

unpinByCipRequest :: Cip -> IpfsMonad (Either ClientError Text)
unpinByCipRequest cip = do
  env <- ask
  liftIO $ runClientM
    (unpinByCip (Just $ envPinataAuthToken env) cip)
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchMetaPinnedRequest :: Text -> IpfsMonad (Either ClientError Files)
fetchMetaPinnedRequest status = do
  env <- ask
  liftIO $ runClientM
    (fetchByStatus (Just $ envPinataAuthToken env) (Just status))
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchByStatusNameRequest :: Text
  -> AssetName
  -> IpfsMonad (Either ClientError Files)
fetchByStatusNameRequest status name = do
  env <- ask
  liftIO $ runClientM
    (fetchByStatusName (Just $ envPinataAuthToken env) (Just status) (Just name))
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchByStatusKeyvalueRequest :: Text
  -> AesKeyHash
  -> IpfsMonad (Either ClientError Files)
fetchByStatusKeyvalueRequest status clientId = do
  env <- ask
  liftIO $ runClientM
    (fetchByStatusKeyvalue (Just $ envPinataAuthToken env) (Just status) (Just clientId))
    (mkClientEnv (envManager env) (envPinataPinHost env))

testAuthenticationRequest :: IpfsMonad (Either ClientError CheckTokenResponse)
testAuthenticationRequest = do
  env <- ask
  liftIO $ runClientM
    (testAuthentication (Just $ envPinataAuthToken env))
    (mkClientEnv (envManager env) (envPinataPinHost env))

-- Utils

-- TODO: remove after debug
token :: TokenToIpfs
token = MkTokenToIpfs
  { pinataContent = MkEncryptedToken "super secret key"
  , pinataMetadata = MkMetadata
      { mName = MkAssetName "tokenName"
      , mKeyvalues = MkMetaOptions "client_id_hash"
      }
  }
