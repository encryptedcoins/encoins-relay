module Main where

import Encoins.Relay.Client.Main (runEncoinsClient)

main :: IO ()
main = runEncoinsClient "relayConfig.json" "../verifier/bulletproof_setup.json"