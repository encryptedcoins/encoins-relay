module Main where

import Encoins.Relay.Verifier.Server

main :: IO ()
main = runVerifierServer "result/testnet-preview/verifierConfig.json"