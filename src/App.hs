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
import           Control.Monad.State              (execState, put)
import           Data.Aeson                       (decode)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.Either                      (fromRight)                       
import           Data.FileEmbed                   (embedFile)
import           Data.Maybe                       (fromJust)
import           Data.Text.Class                  (FromText(..))       
import           Ledger                           (Params (..))
import qualified Ledger.Ada                       as Ada
import           Ledger.Constraints.OffChain      (plutusV2OtherScript)
import           Ledger.Constraints.TxConstraints (mustPayToAddressWithReferenceValidator)

import           PlutusTx.Prelude                 hiding (Semigroup(..), Eq (..), (<$>), unless, mapMaybe, find, toList, fromInteger, mempty, concatMap)
import           Prelude                          (IO, print, (<$>), putStrLn)

import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Server.PostScripts.PostScripts
import           Server.Endpoints.Mint            (processTokens, QueueM(..))
import           Test.OffChain                    (TestTransaction, TestTransactionBuilder, testValidatorHash, testValidator)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)


post :: IO ()
post = postScripts

run :: IO ()
run = unQueueM $ processTokens ["a"]

import           Server.PostScripts.PostScripts
import           Server.Endpoints.Mint (processTokens, QueueM(..))
import qualified Ledger.Ada                       as Ada
import           Ledger.Constraints.TxConstraints (mustPayToAddressWithReferenceValidator)
import           Ledger.Constraints.OffChain (plutusV2OtherScript)

post :: IO ()
post = postScripts

run :: IO ()
run = unQueueM $ processTokens ["a"]

execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
execTxs txs s = selectTxConstructor $ map (`execState` s) txs

runTest :: IO ()
runTest = do
    addrWallet <- getWalletAddr
    putStrLn "Address:"
    print addrWallet
    let (walletPKH, walletSKH) = fromRight (error ()) $ fromJust . bech32ToKeyHashes <$> fromText addrWallet
        protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        addrWallet' = fromRight (error ()) $ fromJust . bech32ToAddress <$> fromText addrWallet
    ct <- currentTime
    utxos <- mconcatMapM getUtxosAt [addrWallet']
    let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
        c = mustPayToAddressWithReferenceValidator addrWallet' testValidatorHash Nothing (Ada.adaValueOf 10)
        l = plutusV2OtherScript testValidator
        txs = (:[]) $ do
            put constrInit {txConstructorResult = Just (l, c)}
        constr = fromJust $ execTxs txs constrInit
        (lookups, cons) = fromJust $ txConstructorResult constr
    putStrLn "Script hash:"
    print testValidatorHash
    putStrLn "Balancing..."
    balancedTx <- balanceTx ledgerParams lookups cons
    print balancedTx
    putStrLn "Signing..."
    signedTx <- signTx balancedTx
    print signedTx
    putStrLn "Submitting..."
    submitTxConfirmed signedTx



-- runTest :: IO ()
-- runTest = do
--     addrWallet <- getWalletAddr
--     putStrLn "Address:"
--     print addrWallet
--     let (walletPKH, walletSKH) = fromRight (error ()) $ fromJust . bech32ToKeyHashes <$> fromText addrWallet
--         protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
--         networkId = Testnet $ NetworkMagic 1097911063
--         ledgerParams = Params def protocolParams networkId
--         addrWallet' = fromRight (error ()) $ fromJust . bech32ToAddress <$> fromText addrWallet
    
--     ct <- currentTime
--     utxos <- mconcatMapM getUtxosAt [addrWallet']
--     let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
--         txs = [testMintTx [emptyByteString]]
--         constr = fromJust $ execTxs txs constrInit
--         (lookups, cons) = fromJust $ txConstructorResult constr

--     putStrLn "Balancing..."
--     balancedTx <- balanceTx ledgerParams lookups cons
--     print balancedTx
--     putStrLn "Signing..."
--     signedTx <- signTx balancedTx
--     print signedTx
--     putStrLn "Submitting..."
--     submitTxConfirmed signedTx