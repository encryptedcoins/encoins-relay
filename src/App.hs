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
{-# LANGUAGE TupleSections              #-}

module App where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId (..))
import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.State              (execState)
import           Data.Aeson                       (decode)
import           Data.Either                      (fromRight)                       
import           Data.Maybe                       (fromJust)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.FileEmbed                   (embedFile)
import           Ledger                           (Params (..))

import           PlutusTx.Prelude                 hiding (Semigroup(..), Eq (..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty, concatMap)
import           Prelude                          (IO, print, (<$>), putStrLn)

import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Test.OffChain                    (TestTransaction, TestTransactionBuilder, testMintTx)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)
import           Data.Text.Class                  (FromText(..))       

execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs

runTest :: IO ()
runTest = do
    addrWallet <- getWalletAddr
    let (walletPKH, walletSKH) = fromRight (error ()) $ fromJust . bech32ToKeyHashes <$> fromText addrWallet
        protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        addrWallet' = fromRight (error ()) $ fromJust . bech32ToAddress <$> fromText addrWallet
    
    ct <- currentTime
    utxos <- mconcatMapM getUtxosAt [addrWallet']
    let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
        txs = [testMintTx [emptyByteString]]
        constr = fromJust $ execTxs txs constrInit
        (lookups, cons) = fromJust $ txConstructorResult constr

    putStrLn "Balancing..."
    balancedTx <- balanceTx ledgerParams lookups cons
    print balancedTx
    putStrLn "Signing..."
    signedTx <- signTx balancedTx
    print signedTx
    putStrLn "Submitting..."
    submitTxConfirmed signedTx