
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Config                    (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Diagnostics               (clientDiagnostics, doDiagnostics, pingDiagnostics, providersDiagnostics)
import           Cardano.Server.EndpointName              (EndpointNames (..))
import           Cardano.Server.Endpoints.Version         (showServerVersion)
import           Cardano.Server.Internal                  (AppT, Env (envActiveEndpoints), loadEnv, runServerM)
import           Cardano.Server.Main                      (RunSettings (..), runServer)
import           Cardano.Server.Utils.Logger              (logMsg)
import           Control.Monad                            (void)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Control.Monad.Reader                     (asks)
import           Data.Default                             (Default (..))
import           Data.Proxy                               (Proxy (..))
import           Encoins.Relay.Server.Config              (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Delegation          (distributeRewards)
import           Encoins.Relay.Server.Endpoints.Tx.Server (QueueRef, newQueueRef, processQueue)
import           Encoins.Relay.Server.Internal            (EncoinsRelayEnv (..))
import           Encoins.Relay.Server.Opts                (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server              (EncoinsApi, embedCreds, encoinsServer, loadEncoinsEnv, serverSetup)
import           Encoins.Relay.Server.Status              (StatusApi)
import           Encoins.Relay.Server.Version             (relayVersion)
import           Servant.Client                           (client)
import           System.Random                            (randomIO)
import           UnliftIO.Concurrent                      (forkIO)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    putStrLn $ showServerVersion relayVersion
    config@Config{cAuxilaryConfig  = EncoinsRelayConfig{..}} <- decodeOrErrorFromFile cardanoServerConfigFp
    let ?creds    = embedCreds
    runWithOpts >>= \case
        Run      -> loadEncoinsEnv config >>= loadEnv config >>= run cNodeFilePath
        Setup    -> loadEncoinsEnv config >>= loadEnv config >>= (`runServerM` serverSetup)
        Reward r -> loadEncoinsEnv config >>= loadEnv config >>= (`runServerM` distributeRewards r)
  where
    run nodeFilePath env = do
        qRef <- newQueueRef
        runServer (encoinsServer nodeFilePath qRef) env def
            { rsBeforeMainLoop    = beforeMainLoop env qRef
            , rsServerDiagnostics = serverDiagnostics
            , rsServerName        = "encoins relay server"
            }

serverDiagnostics :: Int -> AppT EncoinsApi IO ()
serverDiagnostics i = doDiagnostics i $ do
    providersDiagnostics
    EndpointNames activeEndpoints <- asks envActiveEndpoints
    if | "status" `elem` activeEndpoints -> statusDiagnostics
       | "ping"   `elem` activeEndpoints -> pingDiagnostics
       | otherwise -> logMsg
            "Unable to make server diagnostics - \
            \both ping and status endpoints are turned off."
  where
    statusDiagnostics = randomIO >>= clientDiagnostics . client @StatusApi (Proxy @StatusApi)

beforeMainLoop :: Env EncoinsApi -> QueueRef -> AppT EncoinsApi IO ()
beforeMainLoop env qRef = void $ liftIO $ forkIO $ processQueue @EncoinsApi env qRef