{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}

module Test.Reference where

import           Control.Monad           (void)
import qualified Data.Map                as M
import           IO.Wallet               (getWalletAddr)
import qualified Ledger.Ada              as Ada
import           Ledger.Typed.Scripts    (Any)
import           Scripts.Constraints     (postMintingPolicyTx, referenceMintingPolicyTx)
import           Server.Tx               (mkTx)
import           Test.Internal           (runTestM)
import           TestingServer.Main      (TestingServer)
import           TestingServer.OffChain  (testToken)
import           TestingServer.OnChain   (testPolicyV, testPolicy)
import qualified PlutusTx.Prelude        as Plutus
import           Utils.Logger            (HasLogger(..))

postReferenceScript :: IO ()
postReferenceScript = void $ runTestM @TestingServer $ do
    addr <- getWalletAddr
    mkTx @Any [addr]
        [ postMintingPolicyTx 
            ?txWalletAddr 
            testPolicyV 
            (Nothing :: Maybe ())
            (Ada.adaValueOf 20)
        ]

runReferenceTest :: IO ()
runReferenceTest = void $ runTestM @TestingServer $ do
    addr <- getWalletAddr
    logMsg "\n\n\n\t\t\tMINT1:"
    mkTest "token1" addr
    logMsg "\n\n\n\t\t\tMINT2:"
    mkTest "token2" addr
  where
    mkTest token addr = mkTx @Any [addr]
        [ referenceMintingPolicyTx 
            testPolicy
            (head $ M.keys ?txUtxos) 
            ([token] :: [Plutus.BuiltinByteString])
            (Plutus.sum $ map testToken [token])
        ]