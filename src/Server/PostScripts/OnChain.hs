{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TupleSections      #-}

module Server.PostScripts.OnChain where

import           PlutusTx.Prelude
import           Plutus.V2.Ledger.Api
import           Scripts.Constraints  (tokensMinted)

{-# INLINABLE testTokenName #-}
testTokenName :: BuiltinByteString -> TokenName
testTokenName = TokenName

testPolicyCheck :: [BuiltinByteString] -> ScriptContext -> Bool
testPolicyCheck bss ctx = cond1
  where
    names = map testTokenName bss
    cond1 = tokensMinted ctx $ fromList $ map (, 1) names