{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Encoins.Relay.Apps.Ipfs.Config where

import           Encoins.Relay.Apps.Ipfs.Types

import           Cardano.Server.Config         (decodeOrErrorFromFile)
import           Control.Exception             (bracket)
import qualified Data.ByteString               as BS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Katip
import           Network.HTTP.Client           hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.Client                (BaseUrl (..), Scheme (..))
import           System.IO                     (stdout)
import           Text.Pretty.Simple            (pPrint, pShow)

import Data.Text.Lazy.Builder
import Katip.Format.Time
import Katip.Core (intercalateNs, locationToString)
import Katip.Scribes.Handle

withEnvAndLog :: (IpfsEnv -> IO ()) -> IO ()
withEnvAndLog action = do
  config <- getIpfsConfig
  let logEnv = mkLogEnv
        (icEnvironment config)
        (icVerbosity config)
        (icSeverity config)
  bracket logEnv closeScribes $ \le -> do
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
    mkBearer jwtToken = "Bearer " <> jwtToken
    mkUrl :: Text -> BaseUrl
    mkUrl h = BaseUrl Https (T.unpack h) 443 ""

mkLogEnv :: Environment -> Verbosity -> Severity -> IO LogEnv
mkLogEnv katipEnv verbosity severity = do
  handleScribe <- mkHandleScribeWithFormatter
    ipfsLogFormat
    ColorIfTerminal
    stdout
    (permitItem severity)
    verbosity
  initiatedLogEnv <- initLogEnv "IPFS server" katipEnv
  registerScribe
    "stdout"
    handleScribe
    defaultScribeSettings initiatedLogEnv

logWarn :: KatipContext m => Text -> m ()
logWarn = logLocM WarningS . ls

logInfo :: KatipContext m => Text -> m ()
logInfo = logLocM InfoS . ls

logError :: KatipContext m => Text -> m ()
logError = logLocM ErrorS . ls

-- The following logs colorize and format Haskell types.
logWithFormat :: (Show a, KatipContext m) => Severity -> Bool -> a -> m ()
logWithFormat severity withFormat = logLocM severity . format
  where
    format = if withFormat then (ls . pShow) else showLS

logWarnS :: (Show a, KatipContext m) => Bool -> a -> m ()
logWarnS = logWithFormat WarningS

logInfoS :: (Show a, KatipContext m) => Bool -> a -> m ()
logInfoS = logWithFormat InfoS

logErrorS :: (Show a, KatipContext m) => Bool -> a -> m ()
logErrorS = logWithFormat ErrorS

-- Verbosity 0 show time, severity and message
-- Verbosity 1 severity, message and time with picoseconds
-- Verbosity 2 like V1 plus log location
-- Verbosity 3 like V2 plus PID and ThreadId
ipfsLogFormat :: LogItem a => ItemFormatter a
ipfsLogFormat withColor verb Item {..} =
  brackets nowStr
    <> brackets (mconcat $ map fromText $ intercalateNs _itemNamespace)
    <> brackets (fromText (renderSeverity' _itemSeverity))
    <> verbMore V2 mempty ( brackets ("PID " <> fromString (show _itemProcess))
             <> brackets ("ThreadId " <> fromText (getThreadIdText _itemThread))
              )
    <> mconcat ks
    <> verbMore V1 mempty (maybe mempty (brackets . fromString . locationToString) _itemLoc)
    <> fromText " "
    <> (unLogStr _itemMessage)
  where
    nowStr = fromText (verbMore V0 formatAsLogTime formatAsIso8601 $ _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    verbMore v def t = if verb > v then t else def
    renderSeverity' severity =
      colorBySeverityFull withColor severity (renderSeverity severity)

colorBySeverityFull :: Bool -> Severity -> Text -> Text
colorBySeverityFull withColor severity msg = case severity of
  EmergencyS -> red msg
  AlertS -> red msg
  CriticalS -> red msg
  ErrorS -> red msg
  WarningS -> yellow msg
  InfoS -> green msg
  DebugS -> blue msg
  _ -> msg
  where
    red = colorize "31"
    green = colorize "32"
    yellow = colorize "33"
    blue = colorize "34"
    colorize c s
      | withColor = "\ESC[" <> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s