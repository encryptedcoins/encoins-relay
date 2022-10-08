{-# LANGUAGE TypeApplications           #-}

module App where

import           Ledger.Typed.Scripts             (Any)
import           PlutusTx.Prelude                 (emptyByteString)
import           Server.ServerTx   
import           Test.OffChain  

runTest :: IO ()
runTest = mkTxWithConstraints @Any $ [testMintTx [emptyByteString]]