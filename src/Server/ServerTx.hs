{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Server.ServerTx where

import           Cardano.Api.Shelley              (NetworkMagic(..), NetworkId(..))
import           Control.Monad.Extra              (mconcatMapM)
import           Control.Monad.State              (State, execState, MonadIO(..))
import           Common.Logger                    (HasLogger(..), logPretty)
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

type HasTxEnv = 
    ( ?txWalletAddrBech32 :: Text
    , ?txWalletAddr       :: Address
    , ?txUtxos            :: M.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
    )

mkTxWithConstraints :: forall a m.
    ( FromData (DatumType a)
    , ToData   (DatumType a)
    , ToData   (RedeemerType a)
    , MonadIO m
    , HasLogger m
    ) => (HasTxEnv => [State (TxConstructor () a (RedeemerType a) (DatumType a)) ()]) -> m ()
mkTxWithConstraints txs = do
    walletAddrBech32 <- liftIO getWalletAddr
    let walletAddr = case bech32ToAddress <$> fromText walletAddrBech32 of
            Right (Just addr) -> addr
            _                 -> error "Can't get wallet address from bech32 wallet."
    utxos <- liftIO $ mconcatMapM getUtxosAt [walletAddr]
    ct    <- liftIO currentTime

    let ?txWalletAddrBech32 = walletAddrBech32
        ?txWalletAddr       = walletAddr
        ?txUtxos            = utxos

    let (walletPKH, walletSKH) = case bech32ToKeyHashes <$> fromText walletAddrBech32 of
            Right (Just res) -> res
            _                -> error "Can't get key hashes from bech32 wallet."
        protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        constrInit = mkTxConstructor 
            (walletPKH, walletSKH) 
            ct
            ()
            utxos
        constr = fromJust $ selectTxConstructor $ map (`execState` constrInit) txs
        (lookups, cons) = fromJust $ txConstructorResult constr
    logMsg "Balancing..."
    balancedTx <- liftIO $ balanceTx ledgerParams lookups cons
    logPretty balancedTx
    logMsg "Signing..."
    signedTx <- liftIO $ signTx balancedTx
    logPretty signedTx
    logMsg "Submitting..."
    liftIO $ submitTxConfirmed signedTx
    logMsg "Submited."