{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Ping where

import Common.Logger   (HasLogger(logMsg))
import Servant         (type (:>), NoContent(..), JSON, Get)
import Server.Internal (AppM)

type PingApi = "relayRequestPing" :> Get '[JSON] NoContent

pingHandler :: AppM NoContent
pingHandler = NoContent <$ logMsg "Received ping request."