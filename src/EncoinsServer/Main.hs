{-# LANGUAGE LambdaCase #-}

module EncoinsServer.Main where

import           Cardano.Server.Client.Client (runClient)
import           Cardano.Server.Internal      (loadEnv, runServerM)
import           Cardano.Server.Main          (runServer)
import           EncoinsServer.Client         (clientHandle)
import           EncoinsServer.Opts           (ServerMode (..), runWithOpts)
import           EncoinsServer.Server         (mkServerHandle, serverSetup)
import           System.Directory             (createDirectoryIfMissing)

runEncoinsServer :: IO ()
runEncoinsServer = runWithOpts >>= \case
    Run   -> mkServerHandle >>= runServer
    Setup -> mkServerHandle >>= loadEnv >>= (`runServerM` serverSetup)

runEncoinsClient :: IO ()
runEncoinsClient = do
    createDirectoryIfMissing True "secrets"
    mkServerHandle >>= (`runClient` clientHandle)