module Main where

import Encoins.Relay.Verifier.Server

main :: IO ()
main = runVerifierServer "../verifier/verifierConfig.json"
