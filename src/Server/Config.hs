{-# LANGUAGE DeriveGeneric #-}

module Server.Config where
import           Data.Aeson           (FromJSON(parseJSON), eitherDecode, genericParseJSON)
import           Data.Aeson.Casing    (aesonPrefix, snakeCase)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

data Config = Config
    { cServerAddress     :: Text
    , cNodeAddress       :: Text
    , cChainIndexAddress :: Text
    , cMinUtxosAmount    :: Int
    , cAuxiliaryEnvFile  :: FilePath
    , cWalletFile        :: FilePath
    } deriving (Show, Generic)

configFile :: FilePath
configFile = "testnet/config.json"

loadConfig :: IO Config
loadConfig = decodeOrErrorFromFile configFile

instance FromJSON Config where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a
decodeOrErrorFromFile = fmap (either error id . eitherDecode . LBS.fromStrict) . BS.readFile