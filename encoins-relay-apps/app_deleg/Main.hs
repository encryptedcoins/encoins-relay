module Main where

import           Encoins.Relay.Apps.Delegation.Server (runDelegationServer)

main :: IO ()
main = runDelegationServer "delegConfig.json"