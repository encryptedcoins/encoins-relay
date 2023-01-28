{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Tests.EncoinsServer where

import           Control.Monad.IO.Class    (MonadIO(..))
import           Control.Monad.Reader      (asks)
import           ENCOINS.BaseTypes         (MintingPolarity (..))
import           ENCOINS.Bulletproofs      (Input(..))
import           EncoinsServer.Main        (EncoinsServer)
import           IO.Wallet                 (HasWallet(..))
import           Ledger.Ada                (lovelaceOf)
import           PlutusTx.Builtins.Class   (stringToBuiltinByteString)

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

setup :: IO ()
setup = runAppM setupServer