module Main where

import Encoins.Relay.Server.Main (runEncoinsServer)

main :: IO ()
main = runEncoinsServer "result/testnet-preprod/config.json"