{-# LANGUAGE TypeApplications #-}

module Main where

import Client.Main        (startClient)
import EncoinsServer.Main (EncoinsServer)

main :: IO ()
main = startClient @EncoinsServer