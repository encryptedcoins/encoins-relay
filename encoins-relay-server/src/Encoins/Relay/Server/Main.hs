{-# LANGUAGE LambdaCase #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Internal     (loadEnv, runServerM)
import           Cardano.Server.Main         (runServer)
import           Encoins.Relay.Server.Opts   (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server (mkServerHandle, serverSetup)

runEncoinsServer :: IO ()
runEncoinsServer = runWithOpts >>= \case
    Run   -> mkServerHandle >>= runServer
    Setup -> mkServerHandle >>= loadEnv >>= (`runServerM` serverSetup)