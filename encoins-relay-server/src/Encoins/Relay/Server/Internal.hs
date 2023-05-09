{-# LANGUAGE TypeFamilies #-}

module Encoins.Relay.Server.Internal where
    
import           Cardano.Server.Internal                  (AuxillaryEnvOf, ServerM, getAuxillaryEnv)
import           ENCOINS.Core.OnChain                     (EncoinsProtocolParams, encoinsSymbol, ledgerValidatorAddress)
import           Encoins.Relay.Server.Config              (referenceScriptSalt)
import           Ledger                                   (Address, CurrencySymbol, TxOutRef)
import           PlutusAppsExtra.IO.ChainIndex            (getUtxosAt)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Utils.ChainIndex         (MapUTXO)
import           PlutusTx.Builtins                        (BuiltinByteString)
import           Servant.Client                           (ClientEnv)

data EncoinsRelayEnv = EncoinsRelayEnv
    { envRefStakeOwner     :: TxOutRef
    , envRefBeacon         :: TxOutRef
    , envVerifierPKH       :: BuiltinByteString
    , envVerifierClientEnv :: ClientEnv
    }

getTrackedAddresses :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api [Address]
getTrackedAddresses = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    return [ledgerValidatorAddress encoinsProtocolParams, alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api Address
getLedgerAddress = head <$> getTrackedAddresses

getLedgerUtxos :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api MapUTXO
getLedgerUtxos = getLedgerAddress >>= getUtxosAt

getEncoinsProtocolParams :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api EncoinsProtocolParams
getEncoinsProtocolParams = (\e -> (envRefStakeOwner e, envRefBeacon e, envVerifierPKH e)) <$> getAuxillaryEnv

getEncoinsSymbol :: AuxillaryEnvOf api ~ EncoinsRelayEnv => ServerM api CurrencySymbol
getEncoinsSymbol = encoinsSymbol <$> getEncoinsProtocolParams