module Main where

import Encoins.Relay.Apps.Ipfs.Client
import Encoins.Relay.Apps.Ipfs.Server

main :: IO ()
main = do
  putStrLn "ipfs proxy server is running"
  ipfsServer "ipfs_config.json" "pinata_jwt.token"
