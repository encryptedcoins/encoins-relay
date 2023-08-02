{-# LANGUAGE LambdaCase #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Internal     (loadEnv, runServerM)
import           Cardano.Server.Main         (runServer)
import           Encoins.Relay.Server.Opts   (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server (mkServerHandle, serverSetup)
import Cardano.Server.Config (loadConfig)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    c <- loadConfig cardanoServerConfigFp
    runWithOpts >>= \case
        Run   -> mkServerHandle c >>= runServer cardanoServerConfigFp
        Setup -> mkServerHandle c >>= loadEnv cardanoServerConfigFp >>= (`runServerM` serverSetup)