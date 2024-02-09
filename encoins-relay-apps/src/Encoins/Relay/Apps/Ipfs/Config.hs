module Encoins.Relay.Apps.Ipfs.Config where

import           Encoins.Relay.Apps.Ipfs.Types

import           Cardano.Server.Config         (decodeOrErrorFromFile)
import qualified Data.ByteString               as BS
import qualified Data.Text.Encoding            as TE
import           Network.HTTP.Client           hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Text.Pretty.Simple            (pPrint)


getIpfsEnv :: IO IpfsEnv
getIpfsEnv = do
  key <- TE.decodeUtf8 <$> BS.readFile "pinata_jwt.token"
  manager <- newManager tlsManagerSettings
  ipfsConfig <- decodeOrErrorFromFile "ipfs_config.json"
  pPrint ipfsConfig
  pure $ mkIpfsEnv manager key ipfsConfig
