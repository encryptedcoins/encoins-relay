module Main where

import           Encoins.Relay.Apps.Delegation.Server (runDelegationServer)

main :: IO ()
main = runDelegationServer "config.json" "delegation-config.json"