{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Main where

import           Control.Concurrent       (forkIO)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Reader     (ReaderT(runReaderT))
import           Common.Logger            (HasLogger(logMsg))
import           Data.IORef               (newIORef)
import           Data.Sequence            (empty)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  (Proxy(..), type (:<|>)(..), HasServer(ServerT), Context(EmptyContext), hoistServer, serveWithContext,
                                           Handler(runHandler'), Application)
import           Server.Config            (Config(..), loadConfig)
import           Server.Internal          (Env(Env), AppM(unAppM))
import           Server.Endpoints.Balance (BalanceApi, balanceHandler)
import           Server.Endpoints.Mint    (MintApi, mintHandler, processQueue)
import           Server.Endpoints.Ping    (PingApi, pingHandler)
import           Server.Opts              (runWithOpts, Options(..), ServerMode(..))
import           Server.Setup             (setupServer)
import           System.IO                (stdout, BufferMode(LineBuffering), hSetBuffering)

type ServerAPI
    =    PingApi
    :<|> MintApi
    :<|> BalanceApi

server :: ServerT ServerAPI AppM
server = pingHandler
    :<|> mintHandler
    :<|> balanceHandler

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

port :: Int
port = 3000

main :: IO ()
main = do
    Options{..} <- runWithOpts
    conf        <- loadConfig
    case serverMode of
        ServerRun   -> runServer   conf
        ServerSetup -> setupServer conf

runServer :: Config -> IO ()
runServer Config{..} = do
        hSetBuffering stdout LineBuffering
        ref <- newIORef empty
        let env = Env ref confBeaconTxOutRef confWallet
        forkIO $ processQueue env
        logStart env "Starting server..."
        Warp.run port $ mkApp env
    where
        logStart env = runExceptT . runHandler' . flip runReaderT env . unAppM . logMsg

mkApp :: Env -> Application
mkApp env = serveWithContext serverAPI EmptyContext $
    hoistServer serverAPI ((`runReaderT` env) . unAppM) server