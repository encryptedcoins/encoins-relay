{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Test.EncoinsServer where

import Client.Internal           (runClientM)
import Control.Monad.IO.Class    (MonadIO(..))
import Control.Monad.Reader      (asks)
import ENCOINS.Core.BaseTypes    (MintingPolarity (..))
import ENCOINS.Core.Bulletproofs (Input(..))
import EncoinsServer.Main        (EncoinsServer, mkEncoinsRedeemer)
import IO.Wallet                 (HasWallet(getRestoreWallet))
import Ledger.Ada                (Ada)
import PlutusTx.Builtins.Class   (stringToBuiltinByteString)
import Server.Internal           (HasServer(..), envAuxiliary, loadConfig)
import Test.Internal             (runTestM, testBalance, testBalanceAll)

testMintES :: String -> IO ()
testMintES str = testMintEndpointES [(Mint, 2_000_000, str)]

testBurnES :: String -> IO ()
testBurnES str = testMintEndpointES [(Burn, -2_000_000, str)]

testMintEndpointES :: [(MintingPolarity, Ada, String)] -> IO ()
testMintEndpointES args = runTestM @EncoinsServer $ do
        let (ada, inputs) = foldr foldArg (0, []) args
        wallet <- getRestoreWallet
        aEnv <- asks envAuxiliary
        (_, red) <- liftIO $ runClientM aEnv wallet $ mkEncoinsRedeemer (pure (), ada, inputs)
        processTokens red
    where
        foldArg (pol, ada, str) (ada', inputs) = (ada <> ada', Input (stringToBuiltinByteString str) pol : inputs)

testBalanceES :: IO ()
testBalanceES = testBalance @EncoinsServer

testBalanceAllEs :: IO ()
testBalanceAllEs = testBalanceAll @EncoinsServer

setup :: IO ()
setup = runTestM @EncoinsServer $ liftIO loadConfig >>= setupServer @EncoinsServer