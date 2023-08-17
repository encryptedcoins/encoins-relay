module Main where

import qualified Encoins.Relay.Delegation as Delegation

main :: IO ()
main = Delegation.main "delegationConfig.json"