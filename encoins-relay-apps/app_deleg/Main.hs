module Main where

import qualified Encoins.Relay.Apps.Delegation.Main as Delegation

main :: IO ()
main = Delegation.main "delegationConfig.json"