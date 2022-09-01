{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}


module App where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId (..))
import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.State              (execState)
import           Data.Aeson                       (decode)
import           Data.Maybe                       (fromJust)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.FileEmbed                   (embedFile)
import           Ledger                           (Params (..))

import           PlutusTx.Prelude                 hiding (Semigroup(..), Eq (..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty, concatMap)
import           Prelude                          (IO, String, print)

import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed)
import           Test.OffChain                    (TestTransaction, TestTransactionBuilder, testMintTx)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)

execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs

runTest :: IO ()
runTest = do
    let (walletPKH, walletSKH) = fromJust $ bech32ToKeyHashes "addr_test1qrpu49nrwn9afpj7rh8ryz8a528jzxsxw9443sm7jstu7fulj0s2vcd3fyd9q3x6cvgwp9hqkgaf3ghh9wcsjlrjtvjqk20uk0"
        addrWallet  = fromJust $ bech32ToAddress "addr_test1qrpu49nrwn9afpj7rh8ryz8a528jzxsxw9443sm7jstu7fulj0s2vcd3fyd9q3x6cvgwp9hqkgaf3ghh9wcsjlrjtvjqk20uk0"
        protocolParams = fromJust $ decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
    ct <- currentTime
    utxos <- mconcatMapM getUtxosAt [addrWallet]
    let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
        txs = [testMintTx [emptyByteString]]
        constr = fromJust $ execTxs txs constrInit
        (lookups, cons) = fromJust $ txConstructorResult constr

    print ("Balancing..." :: String)
    balancedTx <- balanceTx ledgerParams lookups cons
    print balancedTx
    print ("Signing..." :: String)
    signedTx <- signTx balancedTx
    print signedTx
    print ("Submitting..." :: String)
    submitTxConfirmed signedTx