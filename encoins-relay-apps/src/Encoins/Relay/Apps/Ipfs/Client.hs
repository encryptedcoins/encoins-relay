{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}


module Encoins.Relay.Apps.Ipfs.Client where

import           Encoins.Relay.Apps.Ipfs.ClientApi
import           Encoins.Relay.Apps.Ipfs.Types

import           Cardano.Server.Config             (decodeOrErrorFromFile)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Reader              (MonadReader (ask),
                                                    ReaderT (..))
import qualified Data.ByteString                   as BS
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as TE
import           Network.HTTP.Client               hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.Client

import           Text.Pretty.Simple


ipfsClient :: IO ()
ipfsClient = do
  key <- pinataKey "pinata_jwt_token.txt"
  manager <- newManager tlsManagerSettings
  ipfsConfig <- decodeOrErrorFromFile "ipfs_config.json"
  let env = mkIpfsEnv manager key ipfsConfig
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


-- Requests to Pinata API

pinJsonRequest :: Token -> IpfsMonad (Either ClientError PinJsonResponse)
pinJsonRequest p = do
  env <- ask
  liftIO $ runClientM
    (pinJson (Just $ envPinataAuthToken env) p)
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchByCipRequest :: Text -> IpfsMonad (Either ClientError TokenKey)
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

unpinByCipRequest :: Text -> IpfsMonad (Either ClientError Text)
unpinByCipRequest cip = do
  env <- ask
  liftIO $ runClientM
    (unpinByCip (Just $ envPinataAuthToken env) cip)
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchMetaPinnedRequest :: Text -> IpfsMonad (Either ClientError Files)
fetchMetaPinnedRequest status = do
  env <- ask
  liftIO $ runClientM
    (fetchMetaByStatus (Just $ envPinataAuthToken env) (Just status))
    (mkClientEnv (envManager env) (envPinataPinHost env))

fetchMetaByStatusAndNameRequest :: Text
  -> Text
  -> IpfsMonad (Either ClientError Files)
fetchMetaByStatusAndNameRequest status name = do
  env <- ask
  liftIO $ runClientM
    (fetchMetaByStatusAndName (Just $ envPinataAuthToken env) (Just status) (Just name))
    (mkClientEnv (envManager env) (envPinataPinHost env))

-- Utils

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
