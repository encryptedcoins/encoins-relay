{-# LANGUAGE LambdaCase #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Config       (decodeOrErrorFromFile)
import           Cardano.Server.Internal     (loadEnv, runServerM)
import           Cardano.Server.Main         (runServer)
import           Encoins.Relay.Server.Opts   (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server (mkServerHandle, serverSetup)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    c <- decodeOrErrorFromFile cardanoServerConfigFp
    runWithOpts >>= \case
        Run   -> mkServerHandle c >>= runServer c
        Setup -> mkServerHandle c >>= loadEnv c >>= (`runServerM` serverSetup)