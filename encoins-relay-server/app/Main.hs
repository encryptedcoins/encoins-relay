module Main where

import           Encoins.Relay.Server.Config (initializeConfig)
import           Encoins.Relay.Server.Main   (runEncoinsServer)

main :: IO ()
main = do
    let configFp = "relayConfig.json"
    initializeConfig configFp
    runEncoinsServer configFp
