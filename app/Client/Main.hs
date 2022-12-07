{-# LANGUAGE TypeApplications #-}

module Main where

import Client.Main        (startClient)
import EncoinsServer.Main (EncoinsServer)
import System.Directory   (createDirectoryIfMissing)

main :: IO ()
main = do
    createDirectoryIfMissing True "secrets"
    startClient @EncoinsServer