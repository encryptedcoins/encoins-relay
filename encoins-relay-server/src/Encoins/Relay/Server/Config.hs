{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Server.Config where

import           Cardano.Server.Config         (decodeOrErrorFromFile, Config (..))
import           Data.Aeson                    (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Maybe                    (fromJust)
import           GHC.Generics                  (Generic)
import           Ledger                        (Address, TxOutRef (..))
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)

loadEncoinsRelayConfig :: Config -> IO EncoinsRelayConfig
loadEncoinsRelayConfig c = decodeOrErrorFromFile $ cAuxiliaryEnvFile c

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

data EncoinsRelayConfig = EncoinsRelayConfig
    { cRefStakeOwner  :: TxOutRef
    , cRefBeacon      :: TxOutRef
    , cVerifierConfig :: FilePath
    } deriving (Show, Generic)

instance FromJSON EncoinsRelayConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase