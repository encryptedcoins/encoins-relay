module Main where

import qualified Encoins.Relay.Apps.Delegation.Client

main :: IO ()
main = Encoins.Relay.Apps.Delegation.Client.main "delegConfig.json"