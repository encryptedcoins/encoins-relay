module Encoins.Relay.Client.Main where

import           Cardano.Server.Config        (decodeOrErrorFromFile)
import           Cardano.Server.Client.Client (runClientWithOpts)
import           Encoins.Relay.Client.Client  (mkClientHandle)
import           Encoins.Relay.Client.Opts    (Options (..), extractCommonOptions, runWithOpts)
import           Encoins.Relay.Server.Server  (mkServerHandle)
import           System.Directory             (createDirectoryIfMissing)

runEncoinsClient :: FilePath -> IO ()
runEncoinsClient cardanoServerConfigFp = do
    createDirectoryIfMissing True "secrets"
    c <- decodeOrErrorFromFile cardanoServerConfigFp
    opts <- runWithOpts
    sh   <- mkServerHandle c
    runClientWithOpts cardanoServerConfigFp sh (mkClientHandle $ optsEncoinsMode opts) $ extractCommonOptions opts