module Main where

import Encoins.Relay.Verifier.Server

main :: IO ()
main = runVerifierServer "encoins-relay-test/test/configuration/verifierConfig.json"