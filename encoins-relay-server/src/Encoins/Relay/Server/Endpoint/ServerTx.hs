{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Encoins.Relay.Server.Endpoint.ServerTx where

import           Control.Concurrent                  (putMVar, takeMVar, threadDelay)
import           Control.Exception                   (throw)
import           Control.Monad                       (void, when)
import           Control.Monad.Catch                 (Exception, SomeException, catch, handle, throwM)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Control.Monad.Reader                (MonadReader (..), asks)
import           CSL                                 (TransactionInputs)
import           CSL.Class                           (FromCSL (..))
import           Data.IORef                          (atomicModifyIORef, atomicWriteIORef, readIORef)
import           Data.Maybe                          (fromMaybe)
import           Data.Sequence                       (Seq (..), (|>))
import qualified Data.Text                           as T
import           Data.Time                           (getCurrentTime)
import qualified Data.Time                           as Time
import           Encoins.Common.Log                  (logErrorS, logInfo, logInfoS)
import           Encoins.Common.Transform            (toText)
import           ENCOINS.Core.OffChain               (EncoinsMode (..), delegateTx, encoinsSendTx, encoinsTx)
import           ENCOINS.Core.OnChain                (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Relay.Common.Error          (ConnectionError, IsCardanoServerError (..), Throws)
import           Encoins.Relay.Common.Server.Handler (wrapHandler)
import           Encoins.Relay.Common.Tx             (mkTxErrorH, mkWalletTxOutRefs, submitTx)
import           Encoins.Relay.Common.Tx.Input       (InputContext (..))
import           Encoins.Relay.Server.Config         (treasuryWalletAddress)
import           Encoins.Relay.Server.Internal       (EncoinsRelayEnv (..), EncoinsServerInput (..), EncoinsServerM, Queue, QueueElem (..),
                                                      QueueRef, getEncoinsProtocolParams, getTrackedAddresses, newQueueElem,
                                                      runEncoinsServerM)
import           Encoins.Relay.Verifier.Client       (VerifierClientError (..), verifierClient)
import           Encoins.Relay.Verifier.Server       (VerifierApiError (..))
import           Katip                               (katipAddNamespace)
import           Ledger                              (TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex       (getMapUtxoFromRefs)
import           PlutusAppsExtra.IO.Wallet           (getWalletUtxos)
import           PlutusAppsExtra.IO.Wallet.Internal  (getWalletAddr)
import           PlutusAppsExtra.Types.Tx            (TransactionBuilder, txBuilderRequirements)
import           PlutusAppsExtra.Utils.ChainIndex    (filterCleanUtxos)
import qualified PlutusLedgerApi.V2                  as PV2
import           Servant                             (JSON, NoContent (..), Post, ReqBody, (:>))
import qualified Servant.Client                      as Servant

type ServerTxEndpoint
    = "serverTx"
    :> Throws EncoinsTxApiError
    :> Throws ConnectionError
    :> ReqBody '[JSON] (EncoinsServerInput, TransactionInputs)
    :> Post '[JSON] NoContent

data EncoinsTxApiError
    = CorruptedExternalInputs
    | CorruptedValue
    | VerifierError VerifierApiError
    | UnavailableVerifier Servant.ClientError
    deriving (Show, Exception)

instance IsCardanoServerError EncoinsTxApiError where
    errStatus = \case
        UnavailableVerifier{} -> toEnum 500
        _                     -> toEnum 422
    errMsg = \case
        CorruptedExternalInputs      -> "The request contained corrupted external transaction inputs data."
        CorruptedValue               -> "The request contained corrupted value data."
        VerifierError IncorrectInput -> "The request contained incorrect public input."
        VerifierError IncorrectProof -> "The request contained incorrect proof."
        UnavailableVerifier err      -> "The verifier is unavailable.\nDetails:\n" <> toText err

serverTxEndpointHandler :: (EncoinsServerInput, TransactionInputs) -> EncoinsServerM NoContent
serverTxEndpointHandler req = wrapHandler @ServerTxEndpoint $ do
    logInfo "New serverTx request received:"
    logInfoS True req
    qElem <- processRequest req >>= uncurry newQueueElem
    ref   <- asks envQueueRef
    liftIO $ atomicModifyIORef ref ((,()) . (|> qElem))
    liftIO (takeMVar $ qeMvar qElem) >>= either throwM (const $ return NoContent)
--   where
    -- reThrow (fromException @(TxApiErrorOf api) -> Just txApiErr)  = throwM txApiErr
    -- reThrow (fromException @ConnectionError    -> Just connError) = throwM connError
    -- reThrow err                                                   = throwM err

-- TODO: change return type to context only if possible
processRequest :: (EncoinsServerInput, TransactionInputs) -> EncoinsServerM (EncoinsServerInput, InputContext)
processRequest req = sequence $ case req of
    r@(InputRedeemer ((_, changeAddr, _), _, _, _) mode, _) -> mkContext mode changeAddr <$> r
    s@(InputSending _  _ changeAddr, _)                     -> mkContext WalletMode changeAddr <$> s
    d@(InputDelegation changeAddr _, _)                     -> mkContext WalletMode changeAddr <$> d
  where
    mkContext mode addr inputsCSL = do
        builders <- txBuilders (fst req)
        reqs     <- liftIO $ txBuilderRequirements builders
        case mode of
            WalletMode -> do
                utxos    <- getMapUtxoFromRefs reqs $ fromMaybe (throw CorruptedExternalInputs) (fromCSL inputsCSL)
                pure $ InputContextClient utxos utxos (TxOutRef (PV2.TxId "") 1) addr
            LedgerMode -> do
                utxos    <- getWalletUtxos reqs
                InputContextClient mempty utxos (TxOutRef (PV2.TxId "") 1) <$> getWalletAddr

txBuilders :: EncoinsServerInput -> EncoinsServerM [TransactionBuilder ()]
txBuilders (InputRedeemer red mode) = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    relayWalletAddress <- getWalletAddr
    red' <- verifyRedeemer red
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) encoinsProtocolParams red' mode]

txBuilders (InputSending addr valCSL _) = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    pure [encoinsSendTx encoinsProtocolParams addr $ fromMaybe (throw CorruptedValue) $ fromCSL valCSL]

txBuilders (InputDelegation addr ipAddr) = do
    MkEncoinsRelayEnv{..} <- ask
    pure [delegateTx envDelegationCurrencySymbol envDelegationTokenName addr ipAddr]

verifyRedeemer :: EncoinsRedeemer -> EncoinsServerM EncoinsRedeemerOnChain
verifyRedeemer red = do
    verifierClientEnv <- asks envVerifierClientEnv
    let ?servantClientEnv = verifierClientEnv
    liftIO (verifierClient red) >>= either rethrow pure
  where
    rethrow = \case
        VerifierApiError    verifierErr -> throw $ VerifierError verifierErr
        VerifierClientError clientErr   -> throw $ UnavailableVerifier clientErr

processQueue :: EncoinsRelayEnv -> IO ()
processQueue env = runEncoinsServerM env
    $ katipAddNamespace "queue" $ do
        logInfo "Starting queue handler..."
        neverFall go
  where
    neverFall ma = catch ma $ \(err :: SomeException) -> do
        logErrorS True err
        liftIO $ threadDelay 3_000_000
        neverFall ma
    go = liftIO getCurrentTime >>= checkQueue
    checkQueue t = do
        qRef <- asks envQueueRef
        liftIO (readIORef qRef) >>= \case
            Empty    -> idleQueue t >>= checkQueue
            e :<| es -> processQueueElem qRef e es >> go

idleQueue :: Time.UTCTime -> EncoinsServerM Time.UTCTime
idleQueue st = do
    ct <- liftIO getCurrentTime
    let enoughTimePassed = Time.diffUTCTime ct st > 300
    when enoughTimePassed $ logInfo "No new inputs to process."
    checkForCleanUtxos
    pure $ if enoughTimePassed then ct else st

processQueueElem :: QueueRef -> QueueElem -> Queue -> EncoinsServerM ()
processQueueElem qRef QueueElem{..} elems = handle h $ do
    liftIO $ atomicWriteIORef qRef elems
    logInfo "New input to process:"
    logInfoS True qeInput
    logInfo "Context:"
    logInfoS True qeContext
    checkForCleanUtxos
    addrs <- getTrackedAddresses
    txBuilder <- txBuilders qeInput
    submitTx addrs qeContext txBuilder Nothing
    logInfo "Submited."
    liftIO $ putMVar qeMvar $ Right ()
  where
    h = liftIO . putMVar qeMvar . Left

checkForCleanUtxos :: EncoinsServerM ()
checkForCleanUtxos = mkTxErrorH $ do
    addr       <- getWalletAddr
    cleanUtxos <- length . filterCleanUtxos <$> PlutusAppsExtra.IO.Wallet.getWalletUtxos mempty
    minUtxos   <- asks envMinUtxosNumber
    maxUtxos   <- asks envMaxUtxosNumber
    when (cleanUtxos < minUtxos) $ do
        logInfo $ "Address doesn't has enough clean UTXO's: " <> (T.pack . show $ minUtxos - cleanUtxos)
        void $ mkWalletTxOutRefs addr (maxUtxos - cleanUtxos)
