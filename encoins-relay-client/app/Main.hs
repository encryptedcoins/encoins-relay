module Main where

import Encoins.Relay.Client.Main (runEncoinsClient)

main :: IO ()
main = runEncoinsClient "config.json" "../verifier/bulletproof_setup.json"