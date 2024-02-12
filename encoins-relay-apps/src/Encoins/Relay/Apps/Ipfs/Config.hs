{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Apps.Ipfs.Config where

import           Encoins.Common.Log (mkLogEnv, withLogEnv)
import           Encoins.Common.Constant (space)
import           Encoins.Relay.Apps.Ipfs.Types

import           Cardano.Server.Config         (decodeOrErrorFromFile)
import           Control.Exception             (bracket)
import qualified Data.ByteString               as BS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Text.Lazy.Builder        (fromString, fromText)
import           Katip
import           Katip.Core                    (intercalateNs, locationToString)
import           Katip.Format.Time             (formatAsIso8601,
                                                formatAsLogTime)
import           Katip.Scribes.Handle          (brackets, getKeys)
import           Network.HTTP.Client           hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.Client                (BaseUrl (..), Scheme (..))
import           System.IO                     (stdout)
import           Text.Pretty.Simple            (pPrint, pShow)

withIpfsEnv :: (IpfsEnv -> IO ()) -> IO ()
withIpfsEnv action = do
  config <- getIpfsConfig
  let logEnv = mkLogEnv
        "IPFS"
        (icEnvironment config)
        (icVerbosity config)
        (icSeverity config)
  withLogEnv logEnv $ \le -> do
    key <- TE.decodeUtf8 <$> BS.readFile "pinata_jwt.token"
    manager <- newManager tlsManagerSettings
    let env = mkIpfsEnv manager key config le
    action env

getIpfsConfig :: IO IpfsConfig
getIpfsConfig = do
  ipfsConfig <- decodeOrErrorFromFile "ipfs_config.json"
  pPrint ipfsConfig
  pure ipfsConfig

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
