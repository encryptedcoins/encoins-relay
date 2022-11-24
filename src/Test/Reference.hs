{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Reference where

import qualified Data.Map                as M
import qualified Ledger.Ada              as Ada
import           Ledger.Typed.Scripts    (Any)
import           PlutusTx.Prelude        (emptyByteString)
import           Scripts.Constraints     (postMintingPolicyTx, referenceMintingPolicyTx)
import           Server.Internal         (loadRestoreWallet)
import           Server.Tx               (mkTxWithConstraints)
import           Test.Reference.OffChain (testMintTx, testToken)
import           Test.Reference.OnChain  (testPolicyV, testPolicy)
import           IO.Wallet               (HasWallet(..))
import qualified PlutusTx.Prelude as Plutus

instance HasWallet IO where
    getRestoreWallet = loadRestoreWallet

runTest :: IO ()
runTest = mkTxWithConstraints @Any $ [testMintTx [emptyByteString]]

postReferenceScript :: IO ()
postReferenceScript = do
    mkTxWithConstraints @Any 
        [ postMintingPolicyTx 
            ?txWalletAddr 
            testPolicyV 
            (Nothing :: Maybe ())
            (Ada.adaValueOf 20)
        ]

runReferenceTest :: IO ()
runReferenceTest = do
    putStrLn "\n\n\n\t\t\tMINT1:"
    mkTest "token1"
    putStrLn "\n\n\n\t\t\tMINT2:"
    mkTest "token2"
  where
    mkTest token = mkTxWithConstraints @Any
        [ referenceMintingPolicyTx 
            testPolicy
            (head $ M.keys ?txUtxos) 
            ([token] :: [Plutus.BuiltinByteString])
            (Plutus.sum $ map testToken [token])
        ]