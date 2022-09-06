{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Main where

import Control.Monad.Reader
import Data.IORef
import Data.Sequence
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Server.Internal
import Server.Mint
import Server.Ping
import Control.Concurrent
import System.IO

type ServerAPI 
    =    PingApi 
    :<|> MintApi

server :: ServerT ServerAPI AppM
server =  pingHandler :<|> mintHandler

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

port :: Int
port = 3000

runServer :: IO ()
runServer = do
    hSetBuffering stdout NoBuffering
    ref <- newIORef empty
    logDebug "Starting token query handler..."
    forkIO $ processQueue ref
    let env = Env ref
    logDebug "Starting server..."
    Warp.run port $ mkApp env

mkApp :: Env -> Application
mkApp env = serveWithContext serverAPI EmptyContext $ 
    hoistServer serverAPI (`runReaderT` env) server