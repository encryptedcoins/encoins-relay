{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Test.OnChain where

import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), TypedValidator,
                                                        mkTypedValidator, mkUntypedValidator, mkUntypedMintingPolicy)
import           Plutus.V2.Ledger.Api                 (ScriptContext(..), MintingPolicy, TokenName (..), mkMintingPolicyScript)
import           PlutusTx                             (compile)
import           PlutusTx.AssocMap                    (fromList)
import           PlutusTx.Prelude                     (BuiltinByteString, Bool (..), ($), map)

import           Scripts.Constraints                  (tokensMinted)

------------------------------------- Test Minting Policy --------------------------------------

{-# INLINABLE testTokenName #-}
testTokenName :: BuiltinByteString -> TokenName
testTokenName = TokenName

testPolicyCheck :: [BuiltinByteString] -> ScriptContext -> Bool
testPolicyCheck bss ctx = cond1
  where
    names = map testTokenName bss

    cond1 = tokensMinted ctx $ fromList $ map (, 1) names

testPolicy :: MintingPolicy
testPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy testPolicyCheck ||])

------------------------------------- Test Validator --------------------------------------

data Testing
instance ValidatorTypes Testing where
  type instance DatumType Testing = ()
  type instance RedeemerType Testing = ()

{-# INLINABLE testValidatorCheck #-}
testValidatorCheck :: () -> () -> ScriptContext -> Bool
testValidatorCheck _ _ _ = True

testTypedValidator :: TypedValidator Testing
testTypedValidator = mkTypedValidator @Testing
    $$(PlutusTx.compile [|| testValidatorCheck ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @() @()