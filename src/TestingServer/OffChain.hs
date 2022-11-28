{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module TestingServer.OffChain where

import           Control.Monad.State                  (State)
import           Ledger.Tokens                        (token)
import           Ledger.Typed.Scripts                 (Any)
import           Ledger.Value                         (AssetClass (..))
import           Plutus.Script.Utils.V2.Scripts       (ValidatorHash, validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), validatorScript)
import           Plutus.V2.Ledger.Api                 (Value, Validator, CurrencySymbol)
import           PlutusTx.Prelude 
import           Scripts.Constraints                  (tokensMintedTx)
import           Types.TxConstructor                  (TxConstructor (..))
import           TestingServer.OnChain                (testPolicy, testTokenName, testTypedValidator)

type TestTransaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TestTransactionBuilder = State TestTransaction ()

------------------------------------- Testing Minting Policy --------------------------------------

testCurrencySymbol :: CurrencySymbol
testCurrencySymbol = scriptCurrencySymbol testPolicy

testAssetClass :: BuiltinByteString -> AssetClass
testAssetClass bs = AssetClass (testCurrencySymbol, testTokenName bs)

testToken :: BuiltinByteString -> Value
testToken = token . testAssetClass

testMintTx :: [BuiltinByteString] -> TestTransactionBuilder
testMintTx bss = tokensMintedTx testPolicy bss (sum $ map testToken bss)

------------------------------------- Testing Validator --------------------------------------

testValidator :: Validator
testValidator = validatorScript testTypedValidator

testValidatorHash :: ValidatorHash
testValidatorHash = validatorHash testValidator