{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}

module Server.Config where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson             (FromJSON(..), eitherDecode)
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (fromStrict)
import           Data.FileEmbed         (embedFile)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           IO.Wallet              (RestoreWallet)
import           Ledger                 (TxOutRef)

data Config = Config
    { confServerAddress      :: Text
    , confNodeAddress        :: Text
    , confChainIndexAddress  :: Text
    , confAdaStakingTxOutRef :: TxOutRef
    , confWallet             :: RestoreWallet
    } deriving (Show)

data ConfigFile = ConfigFile
    { cfServerAddress          :: Text
    , cfNodeAddress            :: Text
    , cfChainIndexAddress      :: Text
    , cfAdaStakingTxOutRefFile :: FilePath
    , cfWalletFile             :: FilePath
    } deriving (Show, Generic, FromJSON)

loadConfig :: IO Config
loadConfig = do
    ConfigFile{..} <- decodeOrErrorFromFile "testnet/config.json"
    let confServerAddress     = cfServerAddress
        confNodeAddress       = cfNodeAddress
        confChainIndexAddress = cfChainIndexAddress
    confAdaStakingTxOutRef <- decodeOrErrorFromFile cfAdaStakingTxOutRefFile
    confWallet             <- decodeOrErrorFromFile cfWalletFile
    pure Config{..}
  where
    decodeOrErrorFromFile :: FromJSON a => FilePath -> IO a 
    decodeOrErrorFromFile =  fmap (either error id . eitherDecode  . fromStrict) . BS.readFile 

restoreWalletFromConf :: MonadIO m => m RestoreWallet
restoreWalletFromConf = liftIO $ confWallet <$> loadConfig