{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module App where

import qualified Data.Map              as M
import qualified Ledger.Ada            as Ada
import           Ledger.Typed.Scripts  (Any)
import           PlutusTx.Prelude      (emptyByteString)
import           Scripts.Constraints   (postMintingPolicyTx, referenceMintingPolicyTx)
import           Server.Config         (restoreWalletFromConf)
import           Server.ServerTx       (mkTxWithConstraints)
import           Test.OffChain         (testMintTx, testToken)
import           Test.OnChain          (testPolicyV, testPolicy)
import           IO.Wallet             (HasWallet(..))

import qualified PlutusTx.Prelude as Plutus

instance HasWallet IO where
    getRestoreWallet = restoreWalletFromConf

runTest :: IO ()
runTest = mkTxWithConstraints @Any $ [testMintTx [emptyByteString]]

runReferenceTest :: IO ()
runReferenceTest = do

    putStrLn "\n\n\n\t\t\tPOSTING:"
    mkTxWithConstraints @Any 
        [ postMintingPolicyTx 
            ?txWalletAddr 
            testPolicyV 
            (Nothing :: Maybe ()) 
            (Ada.adaValueOf 20)
        ]

    putStrLn "\n\n\n\t\t\tMINT1:"
    mkTxWithConstraints @Any
        [ referenceMintingPolicyTx 
            testPolicy
            (head $ M.keys ?txUtxos) 
            (["test1"] :: [Plutus.BuiltinByteString])
            (Plutus.sum $ map testToken ["test1"])
        ]

    putStrLn "\n\n\n\t\t\tMINT2:"
    mkTxWithConstraints @Any
        [ referenceMintingPolicyTx 
            testPolicy
            (head $ M.keys ?txUtxos) 
            (["test2"] :: [Plutus.BuiltinByteString])
            (Plutus.sum $ map testToken ["test2"])
        ]

