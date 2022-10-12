{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module App where

import           Ledger.Typed.Scripts (Any)
import           PlutusTx.Prelude     (emptyByteString)
import           Server.Config        (restoreWalletFromConf)
import           Server.ServerTx      (mkTxWithConstraints)
import           Test.OffChain        (testMintTx)
import           IO.Wallet            (HasWallet(..))

instance HasWallet IO where
    getRestoreWallet = restoreWalletFromConf

runTest :: IO ()
runTest = mkTxWithConstraints @Any $ [testMintTx [emptyByteString]]