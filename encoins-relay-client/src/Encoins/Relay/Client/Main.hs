module Encoins.Relay.Client.Main where

import           Cardano.Server.Client.Client (runClientWithOpts)
import           Cardano.Server.Config        (decodeOrErrorFromFile)
import           Encoins.Relay.Client.Client  (mkClientHandle)
import           Encoins.Relay.Client.Opts    (Options (..), extractCommonOptions, runWithOpts)
import           Encoins.Relay.Server.Server  (mkServerHandle)
import           System.Directory             (createDirectoryIfMissing)

runEncoinsClient :: FilePath -> FilePath -> IO ()
runEncoinsClient cardanoServerConfigFp bulletproofSetupFp = do
    createDirectoryIfMissing True "secrets"
    c                <- decodeOrErrorFromFile cardanoServerConfigFp
    bulletproofSetup <- decodeOrErrorFromFile bulletproofSetupFp
    opts             <- runWithOpts
    sh               <- mkServerHandle c
    runClientWithOpts c sh (mkClientHandle bulletproofSetup (optsEncoinsMode opts)) $ extractCommonOptions opts