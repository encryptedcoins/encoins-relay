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
import           PlutusTx.Prelude              (BuiltinByteString, toBuiltin)
import           Text.Hex                      (decodeHex)

loadEncoinsRelayConfig :: Config -> IO EncoinsRelayConfig
loadEncoinsRelayConfig c = decodeOrErrorFromFile $ cAuxiliaryEnvFile c

verifierPKH :: BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"

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