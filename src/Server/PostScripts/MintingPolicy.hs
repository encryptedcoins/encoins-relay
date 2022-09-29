{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Server.PostScripts.MintingPolicy where

import           Cardano.Ledger.Alonzo.Language       (Language(PlutusV2))
import           Control.Monad.State                  (State)
import           Ledger                               (CurrencySymbol, Versioned(..), Value, scriptCurrencySymbol, TxOutRef)
import           Ledger.Tokens                        (token)
import           Ledger.Typed.Scripts                 (Any)
import           Ledger.Value                         (AssetClass(..))
import           PlutusTx                             (compile)
import           PlutusTx.Prelude                     
import           Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes(..))
import           Plutus.V2.Ledger.Api                 (MintingPolicy, mkMintingPolicyScript)
import           Scripts.Constraints                  (referenceMintingPolicyTx)
import           Server.PostScripts.OnChain           (testPolicyCheck, testTokenName)
import           Types.TxConstructor                  (TxConstructor)

type TestTransaction = TxConstructor () Any (RedeemerType Any) (DatumType Any)
type TestTransactionBuilder = State TestTransaction ()

serverMintingPolicy :: Versioned MintingPolicy
serverMintingPolicy = Versioned mintingPolicy PlutusV2

mintingPolicy :: MintingPolicy
mintingPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkUntypedMintingPolicy testPolicyCheck ||])

testToken :: BuiltinByteString -> Value
testToken = token . testAssetClass

testAssetClass :: BuiltinByteString -> AssetClass
testAssetClass bs = AssetClass (testCurrencySymbol, testTokenName bs)

testCurrencySymbol :: CurrencySymbol
testCurrencySymbol = scriptCurrencySymbol serverMintingPolicy

referenceServerPolicy :: TxOutRef -> [BuiltinByteString] -> TestTransactionBuilder
referenceServerPolicy utxoRef bss = referenceMintingPolicyTx utxoRef bss (sum $ map testToken bss)