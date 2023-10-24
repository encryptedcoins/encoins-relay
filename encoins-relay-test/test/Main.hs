module Main where

import           Cardano.Server.Config             (decodeOrErrorFromFile)
import           Cardano.Server.Test.Utils         (withCardanoServer)
import qualified Control.Concurrent                as C
import           Control.Exception                 (bracket, bracket_, try)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO (..))
import qualified Encoins.Relay.DelegationSpec      as Delegation
import           Encoins.Relay.Server.Server       (mkServerHandle)
import qualified Encoins.Relay.Server.ServerSpec   as Server
import qualified Encoins.Relay.Server.StatusSpec   as Status
import           Encoins.Relay.Verifier.Server     (runVerifierServer)
import qualified Encoins.Relay.Verifier.ServerSpec as Verifier
import           System.Directory                  (createDirectoryIfMissing, removeDirectoryRecursive, renameDirectory,
                                                    setCurrentDirectory)
import           Test.Hspec                        (parallel, runIO)

main :: IO ()
main = do
    let configFp = "encoins-relay-test/test/configuration/config.json"
    c <- decodeOrErrorFromFile configFp
    sHandle <- mkServerHandle c
    bracket
        (C.forkIO $ runVerifierServer "encoins-relay-test/test/configuration/verifierConfig.json")
        C.killThread
        $ const $ withCardanoServer configFp sHandle 30 $ do
            runIO $ C.threadDelay 50000
            Status.spec
            Verifier.spec
            Server.spec
            Delegation.spec