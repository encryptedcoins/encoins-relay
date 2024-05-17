module Main where

import qualified Encoins.Relay.Delegation.Client

main :: IO ()
main = Encoins.Relay.Delegation.Client.main "delegConfig.json"