{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Server.Tx where

import           Cardano.Api.Shelley       (NetworkMagic(..), NetworkId(..))
import           Control.Monad.Extra       (mconcatMapM)
import           Control.Monad.State       (State, execState, MonadIO(..))
import           Data.Aeson                (decode)
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Default              (Default(..))
import           Data.FileEmbed            (embedFile)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Ledger                    (Address, ChainIndexTxOut, Params(..), POSIXTime, PubKeyHash, TxOutRef, unPaymentPubKeyHash)
import           PlutusTx                  (FromData(..), ToData(..))
import           Plutus.ChainIndex         (ChainIndexTx)
import           Plutus.Script.Utils.Typed (RedeemerType, DatumType)
import           IO.ChainIndex             (getUtxosAt)
import           IO.Time                   (currentTime)
import           IO.Wallet                 (HasWallet(..), signTx, balanceTx, submitTxConfirmed, getWalletAddr, getWalletAddrBech32, getWalletKeyHashes)
import           Types.TxConstructor       (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Logger              (HasLogger(..), logPretty, logSmth)

type HasTxEnv = 
    ( ?txWalletAddr :: Address
    , ?txWalletPKH  :: PubKeyHash
    , ?txCt         :: POSIXTime
    , ?txUtxos      :: M.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
    )

mkTx :: forall a m.
    ( FromData (DatumType a)
    , ToData   (DatumType a)
    , ToData   (RedeemerType a)
    , Show     (DatumType a)
    , Show     (RedeemerType a)
    , HasWallet m
    , HasLogger m
    ) => (HasTxEnv => [State (TxConstructor a (RedeemerType a) (DatumType a)) ()]) -> m ()
mkTx txs = do
    walletAddrBech32       <- getWalletAddrBech32
    walletAddr             <- getWalletAddr
    (walletPKH, walletSKH) <- getWalletKeyHashes
    utxos <- liftIO $ mconcatMapM getUtxosAt [walletAddr]
    ct    <- liftIO currentTime

    let ?txWalletAddr = walletAddr
        ?txWalletPKH  = unPaymentPubKeyHash walletPKH
        ?txCt         = ct
        ?txUtxos      = utxos

    logMsg $ "Wallet address:\n" <> walletAddrBech32

    let protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        constrInit = mkTxConstructor
            (walletPKH, walletSKH)
            ct
            utxos
        constr = fromJust $ selectTxConstructor $ map (`execState` constrInit) txs
        (lookups, cons) = fromJust $ txConstructorResult constr
    logMsg "\tLookups:"
    logSmth lookups
    logMsg "\tConstraints:"
    logSmth cons

    logMsg "Balancing..."
    balancedTx <- balanceTx ledgerParams lookups cons
    logPretty balancedTx
    logMsg "Signing..."
    signedTx <- signTx balancedTx
    logPretty signedTx
    logMsg "Submitting..."
    submitTxConfirmed signedTx
    logMsg "Submited."