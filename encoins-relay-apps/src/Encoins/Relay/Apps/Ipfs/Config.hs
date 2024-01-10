module Encoins.Relay.Apps.Ipfs.Config where

import           Encoins.Relay.Apps.Ipfs.Types

import qualified Data.Text.Encoding                as TE
import qualified Data.ByteString                   as BS
import           Network.HTTP.Client               hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Cardano.Server.Config             (decodeOrErrorFromFile)



getIpfsEnv :: IO IpfsEnv
getIpfsEnv = do
  key <- TE.decodeUtf8 <$> BS.readFile "pinata_jwt.token"
  manager <- newManager tlsManagerSettings
  ipfsConfig <- decodeOrErrorFromFile "ipfs_config.json"
  pure $ mkIpfsEnv manager key ipfsConfig
