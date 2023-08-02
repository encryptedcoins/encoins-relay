module Main where

import Encoins.Relay.Server.Main (runEncoinsServer)

main :: IO ()
main = runEncoinsServer "result/testnet-preview/config.json"