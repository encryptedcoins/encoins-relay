{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Server.Tx where

import           Cardano.Api.Shelley       (NetworkMagic(..), NetworkId(..))
import           Control.Monad.Extra       (mconcatMapM)
import           Control.Monad.State       (State, get, put, execState, MonadIO(..))
import           Data.Aeson                (decode)
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Default              (Default(..))
import           Data.FileEmbed            (embedFile)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Data.Void                 (Void)
import           Ledger                    (Address, CardanoTx(..), ChainIndexTxOut, Params(..), POSIXTime, PubKeyHash, TxOutRef,
                                            StakePubKeyHash(..), PaymentPubKeyHash (..))
import           Ledger.Ada                (lovelaceValueOf) 
import           Ledger.Constraints        (ScriptLookups, mustPayToPubKey, mustPayToPubKeyAddress)
import           Ledger.Tx.CardanoAPI      (unspentOutputsTx)
import           PlutusTx                  (FromData(..), ToData(..))
import           Plutus.ChainIndex         (ChainIndexTx)
import           Plutus.Script.Utils.Typed (RedeemerType, DatumType)
import           IO.ChainIndex             (getUtxosAt)
import           IO.Time                   (currentTime)
import           IO.Wallet                 (HasWallet(..), signTx, balanceTx, submitTxConfirmed, getWalletAddr,
                                            getWalletAddrBech32, getWalletKeyHashes)
import           Types.TxConstructor       (TxConstructor (..), selectTxConstructor, mkTxConstructor)
import           Utils.Logger              (HasLogger(..), logPretty, logSmth)

type HasTxEnv =
    ( ?txWalletAddr :: Address
    , ?txWalletPKH  :: PubKeyHash
    , ?txWalletSKH  :: Maybe PubKeyHash
    , ?txCt         :: POSIXTime
    , ?txUtxos      :: M.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
    , ?txParams     :: Params
    )

type MkTxConstraints a m =
    ( FromData (DatumType a)
    , ToData   (DatumType a)
    , ToData   (RedeemerType a)
    , Show     (DatumType a)
    , Show     (RedeemerType a)
    , HasWallet m
    , HasLogger m
    )

mkTx :: forall a m. MkTxConstraints a m 
    => [Address]
    -> (HasTxEnv => [State (TxConstructor a (RedeemerType a) (DatumType a)) ()])
    -> m CardanoTx
mkTx utxosAddresses txs = do
    walletAddrBech32       <- getWalletAddrBech32
    walletAddr             <- getWalletAddr
    (walletPKH, walletSKH) <- getWalletKeyHashes
    utxos <- liftIO $ mconcatMapM getUtxosAt utxosAddresses
    ct    <- liftIO currentTime

    let protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId

    let ?txWalletAddr = walletAddr
        ?txWalletPKH  = unPaymentPubKeyHash walletPKH
        ?txWalletSKH  = unStakePubKeyHash <$> walletSKH
        ?txCt         = ct
        ?txUtxos      = utxos
        ?txParams     = ledgerParams

    logMsg $ "Wallet address:\n" <> walletAddrBech32

    let constrInit = mkTxConstructor
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
    return signedTx

mkWalletTxOutRefs :: MkTxConstraints Void m => Address -> Int -> m [TxOutRef]
mkWalletTxOutRefs addr n = do
        signedTx <- mkTx [addr] [constructor]
        let refs = case signedTx of
                EmulatorTx _    -> error "Can not get TxOutRef's from EmulatorTx."
                CardanoApiTx tx -> M.keys $ unspentOutputsTx tx
                Both _ tx       -> M.keys $ unspentOutputsTx tx
        pure refs
    where
        constructor :: HasTxEnv => State (TxConstructor Void i o) ()
        constructor = do
            let pkh = PaymentPubKeyHash ?txWalletPKH
                mbSkh = StakePubKeyHash <$> ?txWalletSKH
                cons = case mbSkh of
                    Just skh -> mconcat $ replicate n $ mustPayToPubKeyAddress pkh skh $ lovelaceValueOf 10_000_000
                    Nothing  -> mustPayToPubKey pkh $ lovelaceValueOf 10_000_000
            constr <- get
            put constr { txConstructorResult = Just (mempty :: ScriptLookups Void, cons) }
