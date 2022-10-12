{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE TemplateHaskell #-}

module Server.Config where

import           Data.Aeson           (eitherDecode)
import           Data.ByteString.Lazy (fromStrict)
import           Data.FileEmbed       (embedFile)
import           Data.Text            (Text)
import           Deriving.Aeson
import           IO.Wallet            (RestoreWallet)
import           Ledger               (TxOutRef)

data Config = Config
    { confServerAddress      :: Text
    , confNodeAddress        :: Text
    , confChainIndexAddress  :: Text
    , confAdaStakingTxOutRef :: TxOutRef
    , confWallet             :: RestoreWallet
    } deriving (Show, Generic)
      deriving (FromJSON) via CustomJSON 
        '[FieldLabelModifier '[StripPrefix "conf", CamelTo "_"]] Config

loadConfig :: Config
loadConfig = either error id $ eitherDecode $ fromStrict $(embedFile "testnet/config.json")

restoreWalletFromConf :: Applicative m => m RestoreWallet
restoreWalletFromConf = pure $ confWallet $ loadConfig