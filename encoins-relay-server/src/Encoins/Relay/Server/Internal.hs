{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.Internal where

import           Cardano.Server.Internal                  (AuxillaryEnvOf, ServerM, getAuxillaryEnv)
import           ENCOINS.Core.OnChain                     (EncoinsProtocolParams, encoinsSymbol, ledgerValidatorAddress)
import           Encoins.Relay.Server.Config              (referenceScriptSalt)
import           Plutus.V2.Ledger.Api                     (Address, CurrencySymbol, TokenName, TxOutRef)
import           PlutusAppsExtra.IO.ChainIndex            (getUtxosAt)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (UtxoRequirements)
import           PlutusAppsExtra.Utils.ChainIndex         (MapUTXO)
import           PlutusTx.Builtins                        (BuiltinByteString)
import           Servant.Client                           (ClientEnv)

data EncoinsRelayEnv = EncoinsRelayEnv
    { envRefStakeOwner            :: TxOutRef
    , envRefBeacon                :: TxOutRef
    , envVerifierPKH              :: BuiltinByteString
    , envVerifierClientEnv        :: ClientEnv
    , envDelegationCurrencySymbol :: CurrencySymbol
    , envDelegationTokenName      :: TokenName
    -- ^ We don't get cs and tokenName directly using encoins-core functions
    -- so that we can mock them in tests and quickly get the delegation result.
    }

getTrackedAddresses :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api [Address]
getTrackedAddresses = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    return [ledgerValidatorAddress encoinsProtocolParams, alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api Address
getLedgerAddress = head <$> getTrackedAddresses

getLedgerUtxos :: AuxillaryEnvOf api ~ EncoinsRelayEnv => UtxoRequirements -> ServerM api MapUTXO
getLedgerUtxos reqs = getLedgerAddress >>= getUtxosAt reqs

getEncoinsProtocolParams :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsProtocolParams
getEncoinsProtocolParams = (\e -> (envRefStakeOwner e, envRefBeacon e, envVerifierPKH e)) <$> getAuxillaryEnv

getEncoinsSymbol :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api CurrencySymbol
getEncoinsSymbol = encoinsSymbol <$> getEncoinsProtocolParams