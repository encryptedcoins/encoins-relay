{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}


module Encoins.Relay.Apps.Save.Ipfs.Request where

-- import           Encoins.Relay.Apps.Save.Ipfs.Api
-- import           Encoins.Relay.Apps.Save.Config (withSaveEnv)
-- import           Encoins.Relay.Apps.Save.Types

-- import           Control.Monad.IO.Class            (MonadIO (liftIO))
-- import           Control.Monad.Reader              (MonadReader (ask))
-- import           Data.Text                         (Text)
-- import           Servant.Client

-- import           Text.Pretty.Simple

-- Requests to Pinata API

-- TODO: Handle ClientError inside of requests

-- pinJsonRequest :: TokenToIpfs -> SaveMonad (Either ClientError PinJsonResponse)
-- pinJsonRequest p = do
--   env <- ask
--   liftIO $ runClientM
--     (pinJson (Just $ envPinataAuthToken env) p)
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- fetchByCipRequest :: Cip -> SaveMonad (Either ClientError EncryptedToken)
-- fetchByCipRequest cip = do
--   env <- ask
--   liftIO $ runClientM
--     (fetchByCip cip)
--     (mkClientEnv (envManager env) (envPinataFetchHost env))

-- fetchMetaAllRequest :: SaveMonad (Either ClientError Files)
-- fetchMetaAllRequest = do
--   env <- ask
--   liftIO $ runClientM
--     (fetchMetaAll $ Just $ envPinataAuthToken env)
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- unpinByCipRequest :: Cip -> SaveMonad (Either ClientError Text)
-- unpinByCipRequest cip = do
--   env <- ask
--   liftIO $ runClientM
--     (unpinByCip (Just $ envPinataAuthToken env) cip)
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- fetchMetaPinnedRequest :: Text -> SaveMonad (Either ClientError Files)
-- fetchMetaPinnedRequest status = do
--   env <- ask
--   liftIO $ runClientM
--     (fetchByStatus (Just $ envPinataAuthToken env) (Just status))
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- fetchByStatusNameRequest :: Text
--   -> AssetName
--   -> SaveMonad (Either ClientError Files)
-- fetchByStatusNameRequest status name = do
--   env <- ask
--   liftIO $ runClientM
--     (fetchByStatusName (Just $ envPinataAuthToken env) (Just status) (Just name))
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- fetchByStatusKeyvalueRequest :: Text
--   -> AesKeyHash
--   -> SaveMonad (Either ClientError Files)
-- fetchByStatusKeyvalueRequest status clientId = do
--   env <- ask
--   liftIO $ runClientM
--     (fetchByStatusKeyvalue
--       (Just $ envPinataAuthToken env)
--       (Just status)
--       (Just $ mkKeyvalueClientId clientId)
--     )
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- fetchByStatusNameKeyvalueRequest :: Text
--   -> AssetName
--   -> AesKeyHash
--   -> SaveMonad (Either ClientError Files)
-- fetchByStatusNameKeyvalueRequest status name clientId = do
--   env <- ask
--   liftIO $ runClientM
--     (fetchByStatusNameKeyvalue
--       (Just $ envPinataAuthToken env)
--       (Just status)
--       (Just name)
--       (Just $ mkKeyvalueClientId clientId))
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- testAuthenticationRequest :: SaveMonad (Either ClientError CheckTokenResponse)
-- testAuthenticationRequest = do
--   env <- ask
--   liftIO $ runClientM
--     (testAuthentication (Just $ envPinataAuthToken env))
--     (mkClientEnv (envManager env) (envPinataPinHost env))

-- -- Just an  example for debug

-- ipfsClient :: IO ()
-- ipfsClient = withSaveEnv $ \env -> do
--   runSaveMonad env $ do
--     -- res <- pinJsonRequest token
--     res <- fetchByStatusNameKeyvalueRequest "pinned"
--          (MkAssetName "9118e4426ad22c2afecc61d8a18677183d26a7e7aa52cb59b5534e99868a095e")
--          (MkAesKeyHash "be2a241998e1908c64b96ee0a230a21537548a5a66c64de96d273bcac8e5f846")
--     pPrint res

-- -- TODO: remove after debug
-- token :: TokenToIpfs
-- token = MkTokenToIpfs
--   { pinataContent = MkEncryptedToken "super secret key"
--   , pinataMetadata = MkMetadata
--       { mName = MkAssetName "tokenName"
--       , mKeyvalues = MkMetaOptions "client_id_hash"
--       }
--   }
