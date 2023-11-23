{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Cardano.Server.Config                  (Config (cAuxiliaryEnvFile, cNetworkId), decodeOrErrorFromFile)
import           Cardano.Server.Test.Utils              (withCardanoServer)
import           Cardano.Server.Utils.Logger            (logger, mutedLogger)
import qualified Control.Concurrent                     as C
import           Control.Exception                      (bracket, bracket_, try)
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Encoins.Relay.Apps.Delegation.Client   (mkDelegationClientEnv)
import           Encoins.Relay.Apps.Delegation.Internal (DelegConfig (cDelegationFolder, cFrequency, cHost, cMaxDelay, cMinTokenAmt, cPort),
                                                         DelegationEnv (DelegationEnv))
import           Encoins.Relay.Apps.Delegation.Server   (runDelegationServer, runDelegationServer')
import qualified Encoins.Relay.DelegationSpec           as Delegation
import           Encoins.Relay.Server.Config            (EncoinsRelayConfig (..))
import           Encoins.Relay.Server.Server            (mkServerHandle)
import qualified Encoins.Relay.Server.ServerSpec        as Server
import qualified Encoins.Relay.Server.StatusSpec        as Status
import           Encoins.Relay.Verifier.Server          (runVerifierServer)
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
    config <- decodeOrErrorFromFile configFp
    relayConfig <- decodeOrErrorFromFile $ cAuxiliaryEnvFile config
    sHandle <- mkServerHandle config

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
    delegConfig <- decodeOrErrorFromFile delegConfigFp
    delegClientEnv <- mkDelegationClientEnv delegConfig
    let ?servantClientEnv = delegClientEnv
    copyFile "encoins-relay-test/test/configuration/blockfrost.token" "blockfrost.token"
    copyFile "encoins-relay-test/test/configuration/maestro.token" "maestro.token"
    let env = DelegationEnv
            mutedLogger
            Nothing
            (cNetworkId config)
            (cHost delegConfig)
            (cPort delegConfig)
            (cDelegationFolder delegConfig)
            (cFrequency delegConfig)
            (cMaxDelay delegConfig)
            (cMinTokenAmt delegConfig)
            (cDelegationCurrencySymbol relayConfig)
            (cDelegationTokenName relayConfig)
            False
    bracket
        (C.forkIO $ runDelegationServer' env)
        (\threadId -> do
            C.killThread threadId
            getCurrentDirectory >>= print
            removeFile "blockfrost.token"
            removeFile "maestro.token"
            removeDirectoryRecursive (cDelegationFolder delegConfig)
        )
        $ const $ (C.threadDelay 30_000_000 >>) $ hspec $ do
            Delegation.spec