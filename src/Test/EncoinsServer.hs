{-# LANGUAGE TypeApplications   #-}

module Test.EncoinsServer where

import Client.Internal           (runClientM)
import Control.Monad.IO.Class    (MonadIO(..))
import Control.Monad.Reader      (asks)
import ENCOINS.Core.BaseTypes    (MintingPolarity (..))
import ENCOINS.Core.Bulletproofs (Input(..))
import EncoinsServer.Main        (EncoinsServer, mkEncoinsRedeemer)
import IO.Wallet                 (HasWallet(getRestoreWallet), getWalletAddr)
import Ledger.Ada                (lovelaceOf)
import PlutusTx.Builtins.Class   (stringToBuiltinByteString)
import Server.Internal           (HasServer(..), envAuxiliary)
import Server.Tx                 (mkWalletTxOutRefs)
import Test.Internal             (runTestM, testBalance, testBalanceAll)
import Utils.Logger              (logSmth)

testES :: [(String, Integer)] -> IO ()
testES args = runTestM @EncoinsServer $ do
        wallet <- getRestoreWallet
        aEnv <- asks envAuxiliary
        (_, red) <- liftIO $ runClientM aEnv wallet $ mkEncoinsRedeemer (pure (), lovelaceOf ada, inputs)
        processTokens red
    where
        ada = sum $ map snd args
        inputs = map toInput args
        toInput (str, a) = Input (stringToBuiltinByteString str) (adaToPolarity a)
        adaToPolarity a = if a > 0 then Mint else Burn
         
testBalanceES :: IO ()
testBalanceES = testBalance @EncoinsServer

testBalanceAllES :: IO ()
testBalanceAllES = testBalanceAll @EncoinsServer

setup :: IO ()
setup = runTestM @EncoinsServer $ setupServer @EncoinsServer

mkRefs :: Int -> IO ()
mkRefs n = runTestM @EncoinsServer $ do
    addr <- getWalletAddr
    refs <- mkWalletTxOutRefs addr n
    logSmth refs