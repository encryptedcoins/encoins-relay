{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Server.Main where

import           Development.GitRev              (gitCommitDate, gitHash)
import           Encoins.Common.Version          (appVersion, showAppVersion)
import           Encoins.Relay.Common            (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Server     (runCardanoServer)
import           Encoins.Relay.Server.Config     (EncoinsRelayConfig (..), initializeConfig)
import           Encoins.Relay.Server.Delegation (distributeRewards)
import           Encoins.Relay.Server.Internal   (loadEncoinsRelayEnv, runEncoinsServerM, runEncoinsServerMHandler)
import           Encoins.Relay.Server.Opts       (ServerMode (..), runWithOpts)
import           Encoins.Relay.Server.Server     (EncoinsApi, beforeMainLoop, encoinsServer, serverSetup)
import           Paths_encoins_relay_server      (version)
import           Say                             (say)

runEncoinsServer :: FilePath -> IO ()
runEncoinsServer configFp = do
    say $ showAppVersion "Relay server" $ appVersion version $(gitHash) $(gitCommitDate)
    initializeConfig configFp
    config <- decodeOrErrorFromFile configFp
    env <- loadEncoinsRelayEnv config
    runWithOpts >>= \case
        Setup    -> runEncoinsServerM env serverSetup
        Reward r -> runEncoinsServerM env $ distributeRewards r
        Run      -> runCardanoServer @EncoinsApi
          (cPort config)
          (runEncoinsServerMHandler env)
          encoinsServer
          beforeMainLoop
