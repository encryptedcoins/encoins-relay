{-# LANGUAGE TypeApplications           #-}

module App where

import           Ledger.Typed.Scripts (Any)
import           PlutusTx.Prelude     (emptyByteString)
import           Server.ServerTx      (mkTxWithConstraints)
import           Test.OffChain        (testMintTx)

runTest :: IO ()
runTest = mkTxWithConstraints @Any $ [testMintTx [emptyByteString]]