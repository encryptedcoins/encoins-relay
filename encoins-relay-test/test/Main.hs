{-# LANGUAGE TypeApplications #-}

module Main where

import           Cardano.Server.Test.Utils         (withCardanoServer)
import qualified Control.Concurrent                as C
import           Control.Exception                 (bracket, bracket_, try)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Encoins.Relay.Server.Server       (mkServerHandle)
import qualified Encoins.Relay.Server.ServerSpec   as Server
import           Encoins.Relay.Verifier.Server     (runVerifierServer)
import qualified Encoins.Relay.Verifier.ServerSpec as Verifier
import           System.Directory                  (createDirectoryIfMissing, removeDirectoryRecursive, renameDirectory)
import           Test.Hspec                        (parallel)

main :: IO ()
main = bracket

    (try @IOError (renameDirectory "secrets" "secrets_")
        >> createDirectoryIfMissing True "secrets" 
        >> liftIO (C.forkIO runVerifierServer))

    (\verifierServerThreadId ->  C.killThread verifierServerThreadId
        >> removeDirectoryRecursive "secrets" 
        >> try @IOError (renameDirectory "secrets_" "secrets"))

    $ const $ do
        sHandle <- mkServerHandle
        withCardanoServer sHandle $ parallel $ do
            Verifier.spec
            Server.spec