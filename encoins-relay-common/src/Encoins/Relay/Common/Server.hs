{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Encoins.Relay.Common.Server where

import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           Servant                     (Proxy (..), ServerT, hoistServer, serve, )
import qualified Servant
import           Control.Exception           (Exception, throw)
import Encoins.Common.Log
    ( logDebug, logDebugS, logError, logErrorS )
import           Katip                       (KatipContext)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           System.IO                   (BufferMode (LineBuffering), hSetBuffering, stdout)

runCardanoServer :: forall api m.
    ( Servant.HasServer api '[]
    , KatipContext m
    ) => Int -> (forall a. m a -> Servant.Handler a) -> ServerT api m -> m () -> IO ()
runCardanoServer port runApp serverApp beforeMainLoop = do
    hSetBuffering stdout LineBuffering
    Warp.runSettings settings app
    -- case (configHyperTextProtocol config, ?creds) of
    --     (HTTP, _)                 -> Warp.runSettings settings app
    --     (HTTPS, Just (cert, key)) -> Warp.runTLS (Warp.tlsSettingsMemory cert key) settings app
    --     (HTTPS, Nothing)          -> error noCredsMsg
    where
        app = corsWithContentType $ serve (Proxy @api) $ hoistServer (Proxy @api) runApp serverApp
        runHandler = fmap (either throw id) . Servant.runHandler
        settings = Warp.setLogger logReceivedRequest
                 $ Warp.setOnException (const logException)
                 $ Warp.setPort port
                 $ Warp.setBeforeMainLoop (runHandler $ runApp beforeMainLoop)
                   Warp.defaultSettings
        logReceivedRequest req status _ = runHandler $ runApp $ do
            logDebug "Received request:"
            logDebugS True req
            logDebug "Status:"
            logDebugS True status
        logException = runHandler . runApp . logCriticalExceptions

corsWithContentType :: Wai.Middleware
corsWithContentType = cors (const $ Just policy)
    where policy = simpleCorsResourcePolicy
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
