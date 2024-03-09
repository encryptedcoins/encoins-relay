{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Encoins.Relay.Server.Internal where

import           Cardano.Api                              (NetworkId)
import           Cardano.Node.Emulator.Params             (Params)
import           Cardano.Server.Config                    (HyperTextProtocol)
import           Cardano.Server.Internal                  (AppT, AuxillaryEnvOf, getAuxillaryEnv)
import           Control.Monad.Catch                      (MonadCatch)
import           Control.Monad.IO.Class                   (MonadIO)
import           Data.Text                                (Text)
import           ENCOINS.Core.OnChain                     (EncoinsProtocolParams, encoinsSymbol, ledgerValidatorAddress)
import           Encoins.Relay.Server.Config              (referenceScriptSalt)
import           Plutus.V2.Ledger.Api                     (Address, CurrencySymbol, TokenName, TxOutRef)
import           PlutusAppsExtra.Api.Blockfrost           (BlockfrostToken)
import           PlutusAppsExtra.Api.Maestro              (MaestroToken)
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndexProvider, getUtxosAt)
import           PlutusAppsExtra.IO.Tx                    (TxProvider)
import           PlutusAppsExtra.IO.Wallet                (RestoredWallet, WalletProvider)
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
    , envDelegationSeverHost      :: Text
    , envDelegationServerPort     :: Int
    , envDelegationServerProtocol :: HyperTextProtocol
    , envDelegationIp             :: Text
    , envNetworkId                :: NetworkId
    , envCollateral               :: Maybe TxOutRef
    , envProtocolParams           :: Params
    , envMinUtxosNumber           :: Int
    , envMaxUtxosNumber           :: Int
    , envWallet                   :: Maybe RestoredWallet
    , envWalletProvider           :: WalletProvider
    , envChainIndexProvider       :: ChainIndexProvider
    , envTxProvider               :: TxProvider
    , envBlockfrostToken          :: Maybe BlockfrostToken
    , envMaestroToken             :: Maybe MaestroToken
    , envDiagnosticsInterval      :: Int
    }

getTrackedAddresses :: (AuxillaryEnvOf api ~ EncoinsRelayEnv, Monad m) => AppT api m [Address]
getTrackedAddresses = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    return [ledgerValidatorAddress encoinsProtocolParams, alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: (AuxillaryEnvOf api ~ EncoinsRelayEnv, Monad m) => AppT api m Address
getLedgerAddress = head <$> getTrackedAddresses

getLedgerUtxos :: (AuxillaryEnvOf api ~ EncoinsRelayEnv, MonadIO m, MonadCatch m) => UtxoRequirements -> AppT api m MapUTXO
getLedgerUtxos reqs = getLedgerAddress >>= getUtxosAt reqs

getEncoinsProtocolParams :: (AuxillaryEnvOf api ~ EncoinsRelayEnv, Monad m) => AppT api m EncoinsProtocolParams
getEncoinsProtocolParams = (\e -> (envRefStakeOwner e, envRefBeacon e, envVerifierPKH e)) <$> getAuxillaryEnv

getEncoinsSymbol :: (AuxillaryEnvOf api ~ EncoinsRelayEnv, Monad m) => AppT api m CurrencySymbol
getEncoinsSymbol = encoinsSymbol <$> getEncoinsProtocolParams
