{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Server.Config where

import           Cardano.Server.Config         (Config (..), decodeOrErrorFromFile)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Plutus.V2.Ledger.Api          (Address, CurrencySymbol, TokenName, TxOutRef (..))
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusTx.Builtins             (BuiltinByteString)

loadEncoinsRelayConfig :: MonadIO m => Config -> m EncoinsRelayConfig
loadEncoinsRelayConfig c = liftIO $ decodeOrErrorFromFile $ cAuxiliaryEnvFile c

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr1q8u2rh5uud6yzmhq0de7vt7p0rvqpfadwnee3tjnz2tl4rct6qt03wjc2lfwyqnd54gwfdey50s7342e3jl6kxwww4kqzfah2x"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

data EncoinsRelayConfig = EncoinsRelayConfig
    -- Relay
    { cRefStakeOwner            :: TxOutRef
    , cRefBeacon                :: TxOutRef
    -- Verifier
    , cVerifierPkh              :: BuiltinByteString
    , cVerifierHost             :: Text
    , cVerifierPort             :: Int
    -- Delegation
    , cDelegationCurrencySymbol :: CurrencySymbol
    , cDelegationTokenName      :: TokenName
    , cDelegationServerHost     :: Text
    , cDelegationServerPort     :: Int
    , cDelegationIp             :: Text
    } deriving (Show, Generic)

instance FromJSON EncoinsRelayConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase