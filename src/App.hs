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
import           Data.Void                        (Void)
import           Ledger                           (Params (..))
import           Ledger.Ada                       (lovelaceValueOf)
import           Ledger.Constraints               (TxConstraints, ScriptLookups, mkTx, mustPayToPubKeyAddress, mustPayToPubKey)

import           PlutusTx.Prelude                 hiding (Semigroup(..), Eq (..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty, concatMap)
import           Prelude                          (IO, String, print, (<$>), putStrLn, replicate, mempty)

import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Test.OffChain                    (TestTransaction, TestTransactionBuilder, testMintTx)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)
import           Data.Text.Class       

execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs

runTest :: IO ()
runTest = do
    addrWallet <- getWalletAddr
    let Right (walletPKH, walletSKH) = fromJust . bech32ToKeyHashes <$> fromText addrWallet
        protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        Right addrWallet' = fromJust . bech32ToAddress <$> fromText addrWallet
    
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