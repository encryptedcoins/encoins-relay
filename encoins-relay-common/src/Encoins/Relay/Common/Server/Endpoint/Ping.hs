{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Encoins.Relay.Common.Server.Endpoint.Ping where

import           Encoins.Common.Log (logInfo)
import           Katip              (KatipContext)
import           Servant            (Get, JSON, NoContent (..), type (:>))

type PingEndpoint = "ping" :> Get '[JSON] NoContent

pingEndpointHandler :: (Monad m, KatipContext m) => m NoContent
pingEndpointHandler = do
    logInfo "Received ping request"
    -- checkEndpointAvailability PingE
    pure NoContent
