{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.EncoinsServer where

import           Client.Internal           (runClientM)
import           Control.Monad.IO.Class    (MonadIO(..))
import           Control.Monad.Reader      (asks)
import qualified Data.Map                  as Map
import           ENCOINS.Core.BaseTypes    (MintingPolarity (..))
import           ENCOINS.Core.Bulletproofs (Input(..))
import           EncoinsServer.Main        (EncoinsServer, mkEncoinsRedeemer)
import           IO.ChainIndex             (getUtxosAt)
import           IO.Wallet                 (HasWallet(getRestoreWallet), getWalletAddr)
import           Ledger.Ada                (lovelaceOf)
import           PlutusTx.Builtins.Class   (stringToBuiltinByteString)
import           Server.Internal           (HasServer(..), envAuxiliary)
import           Server.Tx                 (mkWalletTxOutRefs)
import           Test.Internal             (runTestM, testBalance, testBalanceAll)
import           Utils.ChainIndex          (filterCleanUtxos)
import           Utils.Logger              (HasLogger(..), logSmth, (.<))


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
    addr  <- getWalletAddr
    utxos <- liftIO (getUtxosAt addr)
    refs   <- mkWalletTxOutRefs addr n
    utxos' <- liftIO $ getUtxosAt addr
    logMsg $ "Past utxo's ammount: "          .< Map.size utxos
    logMsg $ "Past clean utxo's ammount: "    .< Map.size (filterCleanUtxos utxos)
    logMsg $ "Current utxo's ammount: "       .< Map.size utxos'
    logMsg $ "Current clean utxo's ammount: " .< Map.size (filterCleanUtxos utxos')
    logSmth refs

