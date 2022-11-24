{-# LANGUAGE TypeApplications #-}

module Test.TestingServer where

import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Server.Internal         (HasServer(processTokens))
import Test.Internal           (runTestM, testBalance, testBalanceAll)
import TestingServer.Main      (TestingServer)

testBalanceTS :: IO ()
testBalanceTS = testBalance @TestingServer

testBalanceAllTS :: IO ()
testBalanceAllTS = testBalanceAll @TestingServer

testMintTS :: [String] -> IO ()
testMintTS = runTestM @TestingServer . processTokens . map stringToBuiltinByteString