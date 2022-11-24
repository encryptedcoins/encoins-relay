{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Main where

import           Control.Concurrent       (forkIO)
import           Control.Monad.Reader     (ReaderT(runReaderT))
import           Utils.Logger             (HasLogger(logMsg))
import           EncoinsServer.Main       (EncoinsServer)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import           Servant                  (Proxy(..), type (:<|>)(..), ServerT, Context(EmptyContext), hoistServer,
                                           serveWithContext, Application)
import           Server.Endpoints.Balance (BalanceApi, balanceHandler)
import           Server.Endpoints.Mint    (HasMintEndpoint, MintApi, mintHandler, processQueue)
import           Server.Endpoints.Ping    (PingApi, pingHandler)
import           Server.Internal          (AppM(unAppM), HasServer(..), loadEnv, Env)
import           Server.Opts              (runWithOpts, Options(..), ServerMode(..), ServerType(..))
import           Server.Setup             (runSetupM)
import           System.IO                (stdout, BufferMode(LineBuffering), hSetBuffering)
import           TestingServer.Main       (TestingServer)

type ServerAPI s
    =    PingApi
    :<|> MintApi s
    :<|> BalanceApi

type ServerConstraints s =
    ( HasMintEndpoint s
    , Servant.HasServer (ServerAPI s) '[]
    )

server :: HasMintEndpoint s => ServerT (ServerAPI s) (AppM s)
server = pingHandler
    :<|> mintHandler
    :<|> balanceHandler

serverAPI :: forall s. Proxy (ServerAPI s)
serverAPI = Proxy @(ServerAPI s)

port :: Int
port = 3000

main :: IO ()
main = do
    Options{..} <- runWithOpts
    case serverType of
        Encoins -> withInstantiation @EncoinsServer
        Test    -> withInstantiation @TestingServer

withInstantiation :: forall s. ServerConstraints s => IO ()
withInstantiation = do
    Options{..} <- runWithOpts
    env        <- loadEnv @s
    case serverMode of
        Run   -> runServer env
        Setup -> runSetupM env $ setupServer @s

runServer :: forall s. ServerConstraints s => Env s -> IO ()
runServer env = do
        hSetBuffering stdout LineBuffering
        forkIO $ processQueue env
        Warp.run port $ mkApp @s env

mkApp :: forall s. ServerConstraints s => Env s -> Application
mkApp env = serveWithContext (serverAPI @s) EmptyContext $
    hoistServer (serverAPI @s) ((`runReaderT` env) . unAppM . (logStart >>)) server
    where
        logStart = logMsg "Starting server..."