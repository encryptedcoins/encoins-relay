{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

module Server.PostScripts.PostScripts where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId (..))
import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.State              (execState)
import           Common.Logger                    (HasLogger(..))
import           Data.Aeson                       (decode)
import           Data.Maybe                       (fromJust)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.FileEmbed                   (embedFile)
import           Ledger                           (Params (..))
import qualified Ledger.Ada                       as Ada
import           Scripts.Constraints              (postMintingPolicyTx)
import           Server.PostScripts.MintingPolicy (serverMintingPolicy, TestTransaction, TestTransactionBuilder)
import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)
import           Data.Text.Class                  (FromText(fromText))

newtype PostScriptsM a = PostScriptsM { unPostScriptsM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger PostScriptsM where
    loggerFilePath = "server.log"

postScripts :: IO ()
postScripts = unPostScriptsM $ do
    bech32 <- liftIO getWalletAddr
    let (walletPKH, walletSKH) = case bech32ToKeyHashes <$> fromText bech32 of
            Right (Just res) -> res
            _                -> error "Can't get key hashes from bech32 wallet."
        protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        addrWallet = case bech32ToAddress <$> fromText bech32 of
            Right (Just addr) -> addr
            _                 -> error "Can't get wallet address from bech32 wallet."
    ct <- liftIO currentTime
    logSmth bech32
    utxos <- liftIO $ mconcatMapM getUtxosAt [addrWallet]
    let constrInit = mkTxConstructor (walletPKH, walletSKH) ct () utxos :: TestTransaction
        txs = [postMintingPolicyTx addrWallet serverMintingPolicy (Nothing :: Maybe ()) (Ada.adaValueOf 10)]
        constr = fromJust $ execTxs txs constrInit
        (lookups, cons) = fromJust $ txConstructorResult constr

    logMsg "Balancing..."
    balancedTx <- liftIO $ balanceTx ledgerParams lookups cons
    logSmth balancedTx
    logMsg "Signing..."
    signedTx <- liftIO $ signTx balancedTx
    logSmth signedTx
    logMsg "Submitting..."
    liftIO $ submitTxConfirmed signedTx
    logMsg "Submited."
  where
    execTxs :: [TestTransactionBuilder] -> TestTransaction -> Maybe TestTransaction
    execTxs txs s = selectTxConstructor $ map (`execState` s) txs