{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Apps.Ipfs.Config where

import           Encoins.Common.Log (mkLogEnv, withLogEnv)
import           Encoins.Common.Constant (space)
import           Encoins.Relay.Apps.Ipfs.Types

import           Cardano.Server.Config         (decodeOrErrorFromFile)
import qualified Data.ByteString               as BS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Katip
import           Network.HTTP.Client           hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.Client                (BaseUrl (..), Scheme (..))
import           Text.Pretty.Simple            (pPrint)

withIpfsEnv :: (IpfsEnv -> IO ()) -> IO ()
withIpfsEnv action = do
  config <- decodeOrErrorFromFile "ipfs_config.json"
  pPrint config
  let logEnv = mkLogEnv
        "IPFS"
        (icEnvironment config)
        (icVerbosity config)
        (icSeverity config)
  withLogEnv logEnv $ \le -> do
    key <- T.strip . TE.decodeUtf8 <$> BS.readFile "pinata_jwt.token"
    manager <- newManager tlsManagerSettings
    let env = mkIpfsEnv manager key config le
    action env

mkIpfsEnv :: Manager -> Text -> IpfsConfig -> LogEnv -> IpfsEnv
mkIpfsEnv manager pinataToken ipfsConfig logEnv = MkIpfsEnv
  { envHyperTextProtocol  = icHyperTextProtocol ipfsConfig
  , envHost               = icHost ipfsConfig
  , envPort               = icPort ipfsConfig
  , envNetworkId          = icNetworkId ipfsConfig
  , envIpfsCurrencySymbol = icIpfsCurrencySymbol ipfsConfig
  , envPinataFetchHost    = mkUrl $ icPinataFetchHost ipfsConfig
  , envPinataPinHost      = mkUrl $ icPinataPinHost ipfsConfig
  , envScheduleDirectory  = icScheduleDirectory ipfsConfig
  , envPinataAuthToken    = mkBearer pinataToken
  , envManager            = manager
  , envLogEnv             = logEnv
  , envKContext           = mempty
  , envKNamespace         = mempty
  , envFormatMessage      = icFormatMessage ipfsConfig
  }
  where
    mkBearer :: Text -> Text
    mkBearer jwtToken = "Bearer" <> space <> jwtToken
    mkUrl :: Text -> BaseUrl
    mkUrl h = BaseUrl Https (T.unpack h) 443 ""
