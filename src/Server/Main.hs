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
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Reader     (ReaderT(runReaderT))
import           Utils.Logger             (HasLogger(logMsg))
import           Data.IORef               (newIORef)
import           Data.Sequence            (empty)
import           EncoinsServer.Main       (EncoinsServer)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import           Servant                  (Proxy(..), type (:<|>)(..), ServerT, Context(EmptyContext), hoistServer,
                                           serveWithContext, Handler(runHandler'), Application)
import           Server.Endpoints.Balance (BalanceApi, balanceHandler)
import           Server.Endpoints.Mint    (HasMintEndpoint, MintApi, mintHandler, processQueue)
import           Server.Endpoints.Ping    (PingApi, pingHandler)
import           Server.Internal          (Env(Env), AppM(unAppM), HasServer(..), Config(..), loadConfig)
import           Server.Opts              (runWithOpts, Options(..), ServerMode(..), ServerType(..))
import           Server.Setup             (SetupM(..))
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
    conf        <- loadConfig @s
    case serverMode of
        Run   -> runServer conf
        Setup -> unSetupM @s $ setupServer conf

runServer :: forall s. ServerConstraints s => Config s -> IO ()
runServer Config{..} = do
        hSetBuffering stdout LineBuffering
        ref <- newIORef empty
        let env = Env ref confWallet confAuxiliaryEnv confMinUtxosAmount
        forkIO $ processQueue env
        logStart env "Starting server..."
        Warp.run port $ mkApp @s env
    where
        logStart env = runExceptT . runHandler' . flip runReaderT env . unAppM . logMsg

mkApp :: forall s. ServerConstraints s => Env s -> Application
mkApp env = serveWithContext (serverAPI @s) EmptyContext $
    hoistServer (serverAPI @s) ((`runReaderT` env) . unAppM) server