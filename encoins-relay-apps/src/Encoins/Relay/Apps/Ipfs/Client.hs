{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}


module Encoins.Relay.Apps.Ipfs.Client where

import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Reader              (MonadReader (ask),
                                                    ReaderT (..))
import qualified Data.ByteString                   as BS
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as TE
import           Encoins.Relay.Apps.Ipfs.ClientApi
import           Network.HTTP.Client               hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.Client

import           Text.Pretty.Simple


ipfsClient :: IO ()
ipfsClient = do
  key <- auth <$> pinataKey "pinata_jwt_token.txt"
  manager <- newManager tlsManagerSettings
  let env = MkIpfsEnv pinUrl fetchUrl key manager
  flip runReaderT env $ do
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

data IpfsEnv = MkIpfsEnv
  { envPinUrl   :: BaseUrl
  , envFetchUrl :: BaseUrl
  , envAuthKey  :: Text
  , envManager  :: Manager
  }

type IpfsMonad a = ReaderT IpfsEnv IO a

-- Requests to Pinata API

-- runClientInReaderT :: IpfsEnv -> ClientM a -> IpfsMonad a
-- runClientInReaderT env action = do
--     let clientReader = hoistClient ipfsApi (flip runReaderT env) action
--     liftIO $ runClientM clientReader

pinJsonRequest :: Token -> IpfsMonad (Either ClientError PinJsonResponse)
pinJsonRequest p = do
  env <- ask
  liftIO $ runClientM
    (pinJson (Just $ envAuthKey env) p)
    (mkClientEnv (envManager env) (envPinUrl env))

fetchByCipRequest :: Text -> IpfsMonad (Either ClientError TokenKey)
fetchByCipRequest cip = do
  env <- ask
  liftIO $ runClientM
    (fetchByCip cip)
    (mkClientEnv (envManager env) (envFetchUrl env))

fetchMetaAllRequest :: IpfsMonad (Either ClientError Files)
fetchMetaAllRequest = do
  env <- ask
  liftIO $ runClientM
    (fetchMetaAll $ Just $ envAuthKey env)
    (mkClientEnv (envManager env) (envPinUrl env))

unpinByCipRequest :: Text -> IpfsMonad (Either ClientError Text)
unpinByCipRequest cip = do
  env <- ask
  liftIO $ runClientM
    (unpinByCip (Just $ envAuthKey env) cip)
    (mkClientEnv (envManager env) (envPinUrl env))

fetchMetaPinnedRequest :: Text -> IpfsMonad (Either ClientError Files)
fetchMetaPinnedRequest status = do
  env <- ask
  liftIO $ runClientM
    (fetchMetaByStatus (Just $ envAuthKey env) (Just status))
    (mkClientEnv (envManager env) (envPinUrl env))

fetchMetaByStatusAndNameRequest :: Text
  -> Text
  -> IpfsMonad (Either ClientError Files)
fetchMetaByStatusAndNameRequest status name = do
  env <- ask
  liftIO $ runClientM
    (fetchMetaByStatusAndName (Just $ envAuthKey env) (Just status) (Just name))
    (mkClientEnv (envManager env) (envPinUrl env))

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
