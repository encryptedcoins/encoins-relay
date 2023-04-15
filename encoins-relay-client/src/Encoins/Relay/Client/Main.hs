module Encoins.Relay.Client.Main where

import           Cardano.Server.Client.Client (runClient)
import           Encoins.Relay.Client.Client  (clientHandle)
import           Encoins.Relay.Server.Server  (mkServerHandle)
import           System.Directory             (createDirectoryIfMissing)

runEncoinsClient :: IO ()
runEncoinsClient = do
    createDirectoryIfMissing True "secrets"
    mkServerHandle >>= (`runClient` clientHandle)