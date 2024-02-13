{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Encoins.Relay.Server.Main where

import           Cardano.Server.Config           (decodeOrErrorFromFile)
import           Cardano.Server.Internal         (loadEnv, runServerM)
import           Cardano.Server.Main             (runServer)
import           Development.GitRev              (gitCommitDate, gitHash)
import           Encoins.Common.Version          (appVersion, showAppVersion)
import           Encoins.Relay.Server.Delegation (distributeRewards)
import           Encoins.Relay.Server.Opts       (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server     (embedCreds, mkServerHandle,
                                                  serverSetup)
import           Paths_encoins_relay_server      (version)
import           Say                             (say)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer cardanoServerConfigFp = do
    say $ showAppVersion "Relay server" $ appVersion version $(gitHash) $(gitCommitDate)
    config <- decodeOrErrorFromFile cardanoServerConfigFp
    let ?creds    = embedCreds
    runWithOpts >>= \case
        Run      -> mkServerHandle config >>= runServer config
        Setup    -> mkServerHandle config >>= loadEnv config >>= (`runServerM` serverSetup)
        Reward r -> mkServerHandle config >>= loadEnv config >>= (`runServerM` distributeRewards r)
