{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module TestingServer.Main (TestingServer) where
    
import Client.Internal         (HasClient(..))
import Control.Monad           (replicateM)
import IO.Wallet               (getWalletAddr)
import Options.Applicative     (argument, metavar, str)
import Plutus.V2.Ledger.Api    (BuiltinByteString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Server.Internal         (HasServer(..))
import Server.Tx               (mkTx)
import System.Random           (randomRIO, randomIO)
import TestingServer.OffChain  (testCurrencySymbol, testMintTx)


data TestingServer

instance HasServer TestingServer where

    type AuxiliaryEnvOf TestingServer = ()

    loadAuxiliaryEnv _ = pure ()

    type RedeemerOf TestingServer = [BuiltinByteString]

    getCurrencySymbol = pure testCurrencySymbol

    processTokens bbs = do
        addr <- getWalletAddr
        mkTx [addr] [testMintTx bbs]

    setupServer _ = pure ()

instance HasClient TestingServer where

    type RequestPieceOf TestingServer = BuiltinByteString

    parseRequestPiece = stringToBuiltinByteString <$> argument str (metavar "token name")

    genRequestPiece = fmap stringToBuiltinByteString $ 
        randomRIO (0, 16) >>= (`replicateM` randomIO)

    mkRedeemer = pure . (pure (),)
