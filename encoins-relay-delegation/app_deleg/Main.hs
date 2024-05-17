module Main where

import           Encoins.Relay.Delegation.Server (runDelegationServer)

main :: IO ()
main = runDelegationServer "delegConfig.json"