{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Ping where

import Servant
import Server.Internal

type PingApi = "relayRequestPing" :> Get '[JSON] NoContent

pingHandler :: AppM NoContent
pingHandler = NoContent <$ logDebug "Recieved ping request."