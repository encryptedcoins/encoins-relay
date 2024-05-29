{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Common.Client where

import           Control.Monad                    (forever)
import           Control.Monad.Catch              (MonadCatch, MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Data                        (Proxy (..))
import qualified Data.Text                        as T
import           GHC.TypeLits                     (KnownSymbol)
import           Servant                          (NoContent)
import           Servant.Client                   (ClientEnv, ClientM, client, runClientM)
import           System.Random                    (randomRIO)
import           Katip
import Encoins.Relay.Common.Server.Endpoint.Ping
import Encoins.Relay.Common.Server.Endpoint.Version
import Encoins.Relay.Common.Server.Handler
import Encoins.Relay.Common.Error
import Encoins.Relay.Common.Client.Opts
import Encoins.Common.Version
import Encoins.Common.Log
import Control.Concurrent (threadDelay)

type HasServantClientEnv = ?servantClientEnv :: ClientEnv

type ClientConstraints e m =
    ( Show (EndpointEnvelope e)
    , KnownSymbol (GetEndpointName e)
    , HasServantClientEnv
    , MonadIO m
    , MonadCatch m
    , KatipContext m
    )

sendRequest :: forall e m. ClientConstraints e m => ClientM (EndpointEnvelope e) -> m (EndpointEnvelope e)
sendRequest endpointClient = do
    logInfo $ "Sending " <> getEndpointName @e <> " request"
    res <- liftIO (runClientM endpointClient ?servantClientEnv)
    logInfo $ "Received response:\n" <> either (T.pack . show) (T.pack . show) res
    either throwM pure res

manualClient :: forall e m. ClientConstraints e m => ClientM (EndpointEnvelope e) -> m (EndpointEnvelope e)
manualClient = sendRequest @e

autoClient :: forall e m. ClientConstraints e m => Interval -> ClientM (EndpointEnvelope e) -> m ()
autoClient i c = forever $ do
    _ <- sendRequest @e c
    d <- randomRIO (1, fromEnum i * 2)
    liftIO $ threadDelay d

autoClientWith :: forall e m b. ClientConstraints e m => m b -> Interval -> (b -> ClientM (EndpointEnvelope e)) -> m ()
autoClientWith gen i c = do
    arg <- gen
    autoClient @e i (c arg)

pingClient :: ClientM (EndpointEnvelope PingEndpoint)
pingClient = client (Proxy @PingEndpoint)

manualPing :: ClientConstraints PingEndpoint m => m NoContent
manualPing = manualClient @PingEndpoint pingClient

autoPing :: ClientConstraints PingEndpoint m => Interval -> m ()
autoPing i = autoClient @PingEndpoint i pingClient

versionClient :: ClientM AppVersion
versionClient = client (Proxy @VersionEndpoint)

manualVersion :: ClientConstraints VersionEndpoint m => m AppVersion
manualVersion = manualClient @VersionEndpoint versionClient

autoVersion :: ClientConstraints VersionEndpoint m => Interval -> m ()
autoVersion i = autoClient @VersionEndpoint i versionClient