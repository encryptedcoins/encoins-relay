{-# LANGUAGE TypeApplications #-}

module Main where

import           System.Directory             (createDirectoryIfMissing)
import           Cardano.Server.Client.Client (startClient)
import           EncoinsServer.Main           (EncoinsServer)

main :: IO ()
main = do
    createDirectoryIfMissing True "secrets"
    startClient @EncoinsServer