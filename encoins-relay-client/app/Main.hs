module Main where

import  Encoins.Relay.Client.Main (runEncoinsClient)

main :: IO ()
main = runEncoinsClient "result/testnet-preprod/config.json"