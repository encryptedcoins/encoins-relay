{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Encoins.Relay.Common.Server.Endpoint.Version where

import           Data.Text              (Text)
import           Data.Version           (Version)
import           Encoins.Common.Log     (logInfo)
import           Encoins.Common.Version (AppVersion, appVersion)
import           Katip                  (KatipContext)
import           Servant                (Get, JSON, type (:>))

type VersionEndpoint = "version"
    :> Get '[JSON] AppVersion

versionEndpointHandler :: (KatipContext m) => Version -> Text -> String -> m AppVersion
versionEndpointHandler version commit time = do
    logInfo "Received version request."
    pure $ appVersion version commit time
