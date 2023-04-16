module Encoins.Relay.Client.Main where

import           Cardano.Server.Client.Client (runClientWithOpts)
import           Encoins.Relay.Client.Client  (mkClientHandle)
import           Encoins.Relay.Client.Opts    (Options (..), extractCommonOptions, runWithOpts)
import           Encoins.Relay.Server.Server  (mkServerHandle)
import           System.Directory             (createDirectoryIfMissing)

runEncoinsClient :: IO ()
runEncoinsClient = do
    createDirectoryIfMissing True "secrets"
    opts <- runWithOpts
    print opts
    sh   <- mkServerHandle
    runClientWithOpts sh (mkClientHandle $ optsEncoinsMode opts) $ extractCommonOptions opts