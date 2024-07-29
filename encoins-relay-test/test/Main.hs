{-# LANGUAGE ImplicitParams #-}

module Main where

import qualified Control.Concurrent                as C
import           Control.Exception                 (bracket, bracket_, try)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Data.IORef                        (newIORef)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Time                         as Time
import           Encoins.Relay.Common              (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Server       (mkServantClientEnv)
import           Encoins.Relay.Server.Config       (EncoinsRelayConfig (..))
import qualified Encoins.Relay.Server.Config       as Relay
import           Encoins.Relay.Server.Main         (runEncoinsServer)
import qualified Encoins.Relay.Server.ServerSpec   as Server
import qualified Encoins.Relay.Server.StatusSpec   as Status
import           Encoins.Relay.Verifier.Server     (VerifierConfig (cHyperTextProtocol), runVerifierServer)
import qualified Encoins.Relay.Verifier.Server     as Verifier
import qualified Encoins.Relay.Verifier.ServerSpec as Verifier
import           System.Directory                  (copyFile, createDirectoryIfMissing, getCurrentDirectory, listDirectory, removeDirectory,
                                                    removeDirectoryRecursive, removeFile, renameDirectory, setCurrentDirectory)
import           Test.Hspec                        (hspec, parallel, runIO)

main :: IO ()
main = do
    let relayConfigFp    = "encoins-relay-test/test/configuration/relayConfig.json"
        verifierConfigFp = "encoins-relay-test/test/configuration/verifierConfig.json"
    relayConfig <- decodeOrErrorFromFile relayConfigFp
    relayClientEnv <- mkServantClientEnv
        (Relay.cPort relayConfig)
        (Relay.cHost relayConfig)
        (Relay.cHyperTextProtocol relayConfig)

    verifierConfig <- decodeOrErrorFromFile verifierConfigFp
    verifierClientEnv <- mkServantClientEnv
        (Verifier.cPort verifierConfig)
        (Verifier.cHost verifierConfig)
        (Verifier.cHyperTextProtocol verifierConfig)

    -- Encoins relay server, delegation server and verifier server specs
    bracket
        (do
            v <- C.forkIO $ runVerifierServer verifierConfigFp
            r <- C.forkIO $ runEncoinsServer relayConfigFp
            pure (v, r)
        )
        (\(v, r) -> do
            C.killThread v
            C.killThread r
        )
        (\_ -> hspec $ do
            let ?servantClientEnv = relayClientEnv
            Status.spec
            Server.spec
            let ?servantClientEnv = verifierClientEnv
            Verifier.spec
        )
