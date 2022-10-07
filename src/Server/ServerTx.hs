{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Server.ServerTx where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId(..))
import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.Reader             (ReaderT, runReaderT, ask)
import           Control.Monad.State              (State, execState, MonadIO(..), lift)
import           Common.Logger                    (HasLogger(..))
import           Data.Aeson                       (decode)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Default                     (Default(..))
import           Data.FileEmbed                   (embedFile)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Ledger                           (Address, ChainIndexTxOut, Params(..), TxOutRef)
import           PlutusTx                         (FromData(..), ToData(..))
import           Plutus.ChainIndex                (ChainIndexTx)
import           Plutus.Script.Utils.Typed        (RedeemerType, DatumType)
import           IO.ChainIndex                    (getUtxosAt)
import           IO.Time                          (currentTime)
import           IO.Wallet                        (signTx, balanceTx, submitTxConfirmed, getWalletAddr)
import           Types.TxConstructor              (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Address                    (bech32ToKeyHashes, bech32ToAddress)
import           Data.Text.Class                  (FromText(fromText))

data TxEnv = TxEnv
    { txEnvWalletAddrBech32 :: Text
    , txEnvWalletAddr       :: Address
    , txEnvUtxos            :: M.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
    }

mkTxEnv :: MonadIO m => m TxEnv
mkTxEnv = liftIO $ do
    txEnvWalletAddrBech32 <- getWalletAddr
    let txEnvWalletAddr = case bech32ToAddress <$> fromText txEnvWalletAddrBech32 of
            Right (Just addr) -> addr
            _                 -> error "Can't get wallet address from bech32 wallet."
    txEnvUtxos <- liftIO $ mconcatMapM getUtxosAt [txEnvWalletAddr]
    pure TxEnv{..}

type ServerTxT m a = ReaderT TxEnv m a

mkTxWithConstraints :: forall a m.
    ( FromData (DatumType a)
    , ToData   (DatumType a)
    , ToData   (RedeemerType a)
    , MonadIO m
    , HasLogger m
    ) => ServerTxT m [State (TxConstructor () a (RedeemerType a) (DatumType a)) ()] -> m ()
mkTxWithConstraints txsM = do
    txEnv <- mkTxEnv
    flip runReaderT txEnv $ do
        TxEnv{..} <- ask
        txs       <- txsM
        ct        <- liftIO currentTime
        let (walletPKH, walletSKH) = case bech32ToKeyHashes <$> fromText txEnvWalletAddrBech32 of
                Right (Just res) -> res
                _                -> error "Can't get key hashes from bech32 wallet."
            protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
            networkId = Testnet $ NetworkMagic 1097911063
            ledgerParams = Params def protocolParams networkId
            constrInit = mkTxConstructor 
                (walletPKH, walletSKH) 
                ct
                ()
                txEnvUtxos
            constr = fromJust $ execTxs txs constrInit
            (lookups, cons) = fromJust $ txConstructorResult constr
        lift $ logMsg "Balancing..."
        balancedTx <- liftIO $ balanceTx ledgerParams lookups cons
        lift $ logSmth balancedTx
        lift $ logMsg "Signing..."
        signedTx <- liftIO $ signTx balancedTx
        lift $ logSmth signedTx
        lift $ logMsg "Submitting..."
        liftIO $ submitTxConfirmed signedTx
        lift $ logMsg "Submited."
  where
    execTxs txs s = selectTxConstructor $ map (`execState` s) txs