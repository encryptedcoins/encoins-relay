{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Cardano.Server.Config                  (Config (cAuxiliaryEnvFile, cNetworkId), decodeOrErrorFromFile)
import           Cardano.Server.Internal                (mkServantClientEnv)
import           Cardano.Server.Main                    (embedCreds)
import           Cardano.Server.Test.Utils              (withCardanoServer)
import           Cardano.Server.Utils.Logger            (mutedLogger)
import qualified Control.Concurrent                     as C
import           Control.Exception                      (bracket, bracket_, try)
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.IORef                             (newIORef)
import qualified Data.Time                              as Time
import           Encoins.Relay.Apps.Delegation.Internal (DelegationEnv (..), Progress (..))
import qualified Encoins.Relay.Apps.Delegation.Internal as Deleg
import           Encoins.Relay.Apps.Delegation.Server   (runDelegationServer, runDelegationServer')
import qualified Encoins.Relay.DelegationSpec           as Delegation
import           Encoins.Relay.Server.Config            (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Server            (mkServerHandle)
import qualified Encoins.Relay.Server.ServerSpec        as Server
import qualified Encoins.Relay.Server.StatusSpec        as Status
import           Encoins.Relay.Verifier.Server          (VerifierConfig (cHyperTextProtocol), runVerifierServer)
import qualified Encoins.Relay.Verifier.ServerSpec      as Verifier
import           System.Directory                       (copyFile, createDirectoryIfMissing, getCurrentDirectory, listDirectory,
                                                         removeDirectory, removeDirectoryRecursive, removeFile, renameDirectory,
                                                         setCurrentDirectory)
import           Test.Hspec                             (hspec, parallel, runIO)

main :: IO ()
main = do
    let configFp         = "encoins-relay-test/test/configuration/config.json"
        verifierConfigFp = "encoins-relay-test/test/configuration/verifierConfig.json"
        delegConfigFp    = "encoins-relay-test/test/configuration/delegConfig.json"
    let ?creds            = embedCreds
    delegConfig <- decodeOrErrorFromFile delegConfigFp
    config      <- decodeOrErrorFromFile configFp
    relayConfig <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
    sHandle     <- mkServerHandle config

    -- -- Encoins relay server and verifier server specs
    -- bracket
    --     (C.forkIO $ runVerifierServer verifierConfigFp)
    --     C.killThread
    --     $ const $ withCardanoServer configFp sHandle 30 $ do
    --         runIO $ C.threadDelay 50000
    --         Status.spec
    --         Verifier.spec
    --         Server.spec

    -- Delegation server specs
    delegClientEnv <- mkServantClientEnv (Deleg.cPort delegConfig) (Deleg.cHost delegConfig) (Deleg.cHyperTextProtocol delegConfig)
    let ?servantClientEnv = delegClientEnv
    setCurrentDirectory "encoins-relay-test/test/configuration"
    resultRef  <- newIORef (Progress Nothing [], Time.UTCTime (toEnum 0) 0)
    balanceRef <- newIORef (mempty, Time.UTCTime (toEnum 0) 0)
    let env = DelegationEnv
            mutedLogger
            Nothing
            (cNetworkId config)
            (Deleg.cHost delegConfig)
            (Deleg.cPort delegConfig)
            (Deleg.cHyperTextProtocol delegConfig)
            (Deleg.cDelegationFolder delegConfig)
            (Deleg.cFrequency delegConfig)
            (Deleg.cMaxDelay delegConfig)
            (Deleg.cMinTokenNumber delegConfig)
            (Deleg.cRewardTokenThreshold delegConfig)
            (cDelegationCurrencySymbol relayConfig)
            (cDelegationTokenName relayConfig)
            False
            resultRef
            balanceRef
    bracket
        (C.forkIO $ runDelegationServer' env)
        (\threadId -> do
            C.killThread threadId
            removeDirectoryRecursive (Deleg.cDelegationFolder delegConfig)
        )
        $ const $ (C.threadDelay 30_000_000 >>) $ hspec $ do
            Delegation.spec