{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Common.Server where

import           Control.Concurrent          (forkIO)
import           Control.Exception           (Exception, throw)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Aeson
import           Data.Default                (Default (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Encoins.Common.Log          (logDebug, logDebugS, logError, logErrorS)
import           GHC.Generics                (Generic)
import           Katip                       (KatipContext)
import           Network.HTTP.Client         (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS     (mkManagerSettings)
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           Servant                     (Proxy (..), ServerT, hoistServer, serve)
import qualified Servant
import qualified Servant.Client              as Servant
import           System.IO                   (BufferMode (LineBuffering), hSetBuffering, stdout)

runCardanoServer :: forall api m.
    ( Servant.HasServer api '[]
    , KatipContext m
    ) => Int -> (forall a. m a -> Servant.Handler a) -> ServerT api m -> m () -> IO ()
runCardanoServer port runApp serverApp beforeMainLoop = do
    hSetBuffering stdout LineBuffering
    _ <- forkIO $ runHandler $ runApp beforeMainLoop
    Warp.runSettings settings app
  where
    app = corsWithContentType $ serve (Proxy @api) $ hoistServer (Proxy @api) runApp serverApp
    runHandler = fmap (either throw id) . Servant.runHandler
    settings = Warp.setLogger logReceivedRequest
             $ Warp.setOnException (const logException)
             $ Warp.setPort port
               Warp.defaultSettings
    logReceivedRequest req status _ = runHandler $ runApp $ do
        logDebug "Received request:"
        logDebugS True req
        logDebug "Status:"
        logDebugS True status
    logException = runHandler . runApp . logCriticalExceptions

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        }

-- This function helps to skip non-critical exceptions (most of them are related to warp/wai) and log only critical ones
logCriticalExceptions :: forall e m. (Exception e, KatipContext m) => e -> m ()
logCriticalExceptions e
    | show e == "Warp: Client closed connection prematurely" = pure ()
    | show e == "Thread killed by timeout manager"           = pure ()
    | otherwise = do
        logError "Unhandled exception:"
        logErrorS True e

data HyperTextProtocol = HTTP | HTTPS
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

schemeFromProtocol :: HyperTextProtocol -> Servant.Scheme
schemeFromProtocol = \case
    HTTP  -> Servant.Http
    HTTPS -> Servant.Https

mkServantClientEnv :: (MonadIO m) => Int -> Text -> HyperTextProtocol -> m Servant.ClientEnv
mkServantClientEnv port host protocol = do
    manager <- liftIO $ newManager $ case protocol of
        HTTP  -> defaultManagerSettings
        HTTPS -> mkManagerSettings def def
    pure $ Servant.ClientEnv
        manager
        (Servant.BaseUrl (schemeFromProtocol protocol) (T.unpack host) port "")
        Nothing
        Servant.defaultMakeClientRequest
