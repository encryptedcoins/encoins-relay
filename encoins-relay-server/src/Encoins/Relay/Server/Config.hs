{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Server.Config where

import           Cardano.Api                   (NetworkId)
import           Cardano.Server.Config         (HyperTextProtocol)
import           Data.Aeson                    (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Plutus.V2.Ledger.Api          (Address, CurrencySymbol, TokenName, TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex (ChainIndexProvider)
import           PlutusAppsExtra.IO.Tx         (TxProvider)
import           PlutusAppsExtra.IO.Wallet     (WalletProvider)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusTx.Builtins             (BuiltinByteString)

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
    , cVerifierProtocol         :: HyperTextProtocol
    -- Delegation
    , cDelegationCurrencySymbol :: CurrencySymbol
    , cDelegationTokenName      :: TokenName
    , cDelegationServerHost     :: Text
    , cDelegationServerPort     :: Int
    , cDelegationServerProtocol :: HyperTextProtocol
    , cDelegationIp             :: Text
    -- Tx stuff
    , cNetworkId                :: NetworkId
    , cCollateral               :: Maybe TxOutRef
    , cProtocolParametersFile   :: FilePath
    , cSlotConfigFile           :: FilePath
    , cMinUtxosNumber           :: Int
    , cMaxUtxosNumber           :: Int
    , cNodeFilePath             :: FilePath
    , cWalletFile               :: Maybe FilePath
    , cBlockfrostTokenFilePath  :: Maybe FilePath
    , cMaestroTokenFilePath     :: Maybe FilePath
    , cWalletProvider           :: Maybe WalletProvider
    , cChainIndexProvider       :: Maybe ChainIndexProvider
    , cTxProvider               :: Maybe TxProvider
    , cDiagnosticsInteval       :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON EncoinsRelayConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase