{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Config           (decodeOrErrorFromFile)
import           Cardano.Server.Internal         (loadEnv, runServerM)
import           Cardano.Server.Main             (runServer)
-- import           Encoins.Relay.Server.Delegation (distributeRewards)
import           Encoins.Relay.Server.Opts       (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server     (mkServerHandle, serverSetup)
import           Encoins.Relay.Server.Version    (relayVersion, showRelayVersion)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    putStrLn $ showRelayVersion relayVersion
    config           <- decodeOrErrorFromFile cardanoServerConfigFp
    runWithOpts >>= \case
        Run      -> mkServerHandle config >>= runServer config
        Setup    -> mkServerHandle config >>= loadEnv config >>= (`runServerM` serverSetup)
        -- Reward r -> mkServerHandle config >>= loadEnv config >>= (`runServerM` distributeRewards config r)