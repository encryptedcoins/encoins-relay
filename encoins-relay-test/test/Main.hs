{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Cardano.Server.Config                  (Config (..), decodeOrErrorFromFile)
import           Cardano.Server.Internal                (mkServantClientEnv, Env (envAuxilaryEnv), loadEnv)
import           Cardano.Server.Main                    (runServer, embedCreds)
import           Cardano.Server.Test.Utils              (withCardanoServer)
import           Cardano.Server.Utils.Logger            (mutedLogger)
import qualified Control.Concurrent                     as C
import           Control.Exception                      (bracket, bracket_, try)
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.IORef                             (newIORef)
import           Data.Maybe                             (fromMaybe)
import qualified Data.Time                              as Time
import           Encoins.Relay.Apps.Delegation.Internal (DelegationEnv (..), Progress (..), DelegationConfig (..), loadDelegationEnv)
import qualified Encoins.Relay.Apps.Delegation.Internal as Deleg
import           Encoins.Relay.Apps.Delegation.Server   (delegationRunSettings, delegationServer, runDelegationServer, DelegApi)
import qualified Encoins.Relay.DelegationSpec           as Delegation
import           Encoins.Relay.Server.Config            (EncoinsRelayConfig (..))
import qualified Encoins.Relay.Server.ServerSpec        as Server
import qualified Encoins.Relay.Server.StatusSpec        as Status
import qualified Encoins.Relay.Server.UtxosSpec         as Utxos
import           Encoins.Relay.Verifier.Server          (runVerifierServer)
import qualified Encoins.Relay.Verifier.ServerSpec      as Verifier
import           System.Directory                       (copyFile, createDirectoryIfMissing, getCurrentDirectory, listDirectory,
                                                         removeDirectory, removeDirectoryRecursive, removeFile, renameDirectory,
                                                         setCurrentDirectory)
import           Test.Hspec                             (hspec, parallel, runIO)
import Encoins.Relay.Server.Server (EncoinsApi, loadEncoinsEnv, encoinsServer)
import qualified Encoins.Relay.Server.Main as Relay
import Encoins.Relay.Server.Endpoints.Tx.Server (newQueueRef)
import qualified Encoins.Relay.Server.SumbitTxSpec as SubmitTx
import qualified Encoins.Relay.Server.ServerSpec as ServerTx

main :: IO ()
main = do
    let configFp          = "encoins-relay-test/test/configuration/config.json"
        verifierConfigFp  = "encoins-relay-test/test/configuration/verifierConfig.json"
        delegConfigFp     = "encoins-relay-test/test/configuration/delegConfig.json"
        blockfrostTokenFp = "encoins-relay-test/test/configuration/blockfrost.token"
        maestroTokenFp    = "encoins-relay-test/test/configuration/maestro.token"



    let ?creds            = embedCreds
    -- delegConfig <- decodeOrErrorFromFile @(Config DelegApi) delegConfigFp
    relayConfig <- decodeOrErrorFromFile @(Config EncoinsApi) configFp
    let 
        -- auxDelegConfig = cAuxilaryConfig delegConfig
        auxRelayConfig = cAuxilaryConfig relayConfig
    auxRelayEnv <- loadEncoinsEnv relayConfig
    relayEnv <- loadEnv relayConfig auxRelayEnv
    qRef <- newQueueRef
    -- Encoins relay server and verifier server specs
    bracket
        (C.forkIO $ runVerifierServer verifierConfigFp)
        C.killThread
        $ const $ withCardanoServer
            configFp
            (encoinsServer (cNodeFilePath auxRelayConfig) qRef)
            (Relay.beforeMainLoop relayEnv qRef)
            auxRelayEnv
            30 $ do
                runIO $ C.threadDelay 50000
                Status.spec
                Verifier.spec
                Server.spec
                Utxos.spec
                ServerTx.spec
                SubmitTx.spec

    -- -- Delegation server specs
    -- delegClientEnv <- mkServantClientEnv (cPort delegConfig) (cHost delegConfig) (cHyperTextProtocol delegConfig)
    -- let ?servantClientEnv = delegClientEnv
    -- setCurrentDirectory "encoins-relay-test/test/configuration"
    -- envProgress <- newIORef (Progress Nothing [], Time.UTCTime (toEnum 0) 0)
    -- -- envTokenBalance <- newIORef (mempty, Time.UTCTime (toEnum 0) 0)
    -- -- envBlockfrostToken <- decodeOrErrorFromFile $
    -- --     fromMaybe blockfrostTokenFp $ Deleg.cMaestroTokenFilePath auxDelegConfig
    -- -- envMaestroToken    <- decodeOrErrorFromFile $
    -- --     fromMaybe maestroTokenFp    $ Deleg.cMaestroTokenFilePath auxDelegConfig
    -- auxDelegEnv@DelegationEnv { envCheckSig = False}  <- loadDelegationEnv auxDelegConfig
    -- delegEnv <- loadEnv delegConfig auxDelegEnv



    -- bracket
    --     (C.forkIO $ runServer @DelegApi delegationServer delegEnv delegationRunSettings)
    --     (\threadId -> do
    --         C.killThread threadId
    --         removeDirectoryRecursive (Deleg.cDelegationFolder auxDelegConfig)
    --     )
    --     $ const $ (C.threadDelay 30_000_000 >>) $ hspec $ do
    --         Delegation.spec