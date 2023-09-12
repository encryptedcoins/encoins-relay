{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Server.Config where

import           Cardano.Server.Config         (Config (..), decodeOrErrorFromFile)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Maybe                    (fromJust)
import           GHC.Generics                  (Generic)
import           Ledger                        (Address, CurrencySymbol, Slot, TokenName, TxOutRef (..))
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)

loadEncoinsRelayConfig :: MonadIO m => Config -> m EncoinsRelayConfig
loadEncoinsRelayConfig c = liftIO $ decodeOrErrorFromFile $ cAuxiliaryEnvFile c

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

data EncoinsRelayConfig = EncoinsRelayConfig
    { cRefStakeOwner            :: TxOutRef
    , cRefBeacon                :: TxOutRef
    , cVerifierConfig           :: FilePath
    , cDelegationFolder         :: FilePath
    , cDelegationMinTokenAmt    :: Integer
    , cDelegationStart          :: Slot
    , cDelegationCurrencySymbol :: CurrencySymbol
    , cDelegationTokenName      :: TokenName
    } deriving (Show, Generic)

instance FromJSON EncoinsRelayConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase