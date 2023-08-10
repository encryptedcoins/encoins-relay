module Main where

import Encoins.Relay.Server.Main (runEncoinsServer)

main :: IO ()
main = runEncoinsServer "encoins-relay-test/test/configuration/config.json"