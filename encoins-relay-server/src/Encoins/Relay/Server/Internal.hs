{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Encoins.Relay.Server.Internal where

import qualified CSL
import           Cardano.Node.Emulator                    (Params (..), pParamsFromProtocolParams)
import           Control.Concurrent                       (MVar, newEmptyMVar)
import           Control.Exception                        (throw)
import           Control.Lens                             ((^?))
import           Control.Monad.Catch                      (Exception, MonadCatch, MonadThrow (..), SomeException)
import           Control.Monad.Except                     (MonadError)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Control.Monad.Reader                     (MonadReader, ReaderT (..), asks, local)
import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Aeson                               as J
import           Data.Aeson.Lens                          (key)
import           Data.Fixed                               (Micro)
import           Data.Functor                             ((<&>))
import           Data.IORef                               (IORef, newIORef)
import           Data.Maybe                               (fromMaybe)
import           Data.Sequence                            (Seq)
import qualified Data.Sequence                            as Seq
import           Data.Text                                (Text)
import           ENCOINS.Core.OffChain                    (EncoinsMode (..))
import           ENCOINS.Core.OnChain                     (EncoinsProtocolParams, EncoinsRedeemer, encoinsSymbol, ledgerValidatorAddress)
import           Encoins.Common.Log                       (mkLogEnv)
import           Encoins.Relay.Common                     (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Server              (HyperTextProtocol, mkServantClientEnv)
import           Encoins.Relay.Common.Server.Handler      (EndpointNames, HasSwitchedEndpoints (..))
import           Encoins.Relay.Common.Tx                  (HasMkTxEnv (..))
import           Encoins.Relay.Common.Tx.Input            (InputContext)
import           Encoins.Relay.Common.Wallet              (loadWallet)
import           Encoins.Relay.Server.Config              (EncoinsApiTF, EncoinsRelayConfig (..), dataProvidersFromConfigDataProviders,
                                                           referenceScriptSalt)
import           GHC.Generics                             (Generic)
import           GHC.Stack                                (HasCallStack)
import           Katip                                    (Katip (..), KatipContext (..), LogContexts, LogEnv, Namespace)
import           Plutus.V2.Ledger.Api                     (Address, CurrencySymbol, TokenName, TxOutRef)
import           PlutusAppsExtra.Api.Blockfrost           (BlockfrostToken)
import           PlutusAppsExtra.Api.Maestro              (MaestroToken, MonadMaestro (..))
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndexProvider, HasChainIndexProvider (..), getUtxosAt)
import           PlutusAppsExtra.IO.Tx                    (HasTxProvider (getTxProvider), TxProvider)
import           PlutusAppsExtra.IO.Wallet                (HasWallet (..), HasWalletProvider (getWalletProvider), RestoredWallet,
                                                           WalletProvider)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (UtxoRequirements)
import           PlutusAppsExtra.Utils.ChainIndex         (MapUTXO)
import           PlutusAppsExtra.Utils.Network            (HasNetworkId (..))
import           PlutusTx.Builtins                        (BuiltinByteString)
import           Servant                                  (Handler)
import qualified Servant
import           Servant.Client                           (ClientEnv)

newtype EncoinsServerM a = MkEncoinsServerM {unEncoinsServerM :: ReaderT EncoinsRelayEnv Handler a}
     deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader EncoinsRelayEnv
        , MonadError Servant.ServerError
        , MonadThrow
        , MonadCatch
        )

runEncoinsServerMHandler :: EncoinsRelayEnv -> EncoinsServerM a -> Handler a
runEncoinsServerMHandler env = (`runReaderT` env) . unEncoinsServerM

runEncoinsServerM :: EncoinsRelayEnv -> EncoinsServerM a -> IO a
runEncoinsServerM env = fmap (either throw id) . Servant.runHandler . runEncoinsServerMHandler env

data EncoinsRelayEnv = MkEncoinsRelayEnv
    -- Server
    { envPort                     :: Int
    , envHost                     :: Text
    , envHyperTextProtocol        :: HyperTextProtocol
    , envActiveEndpoints          :: EndpointNames EncoinsApiTF
    , envDiagnosticsInterval      :: Micro

    -- Encoins server
    , envQueueRef                 :: QueueRef
    , envRefStakeOwner            :: TxOutRef
    , envRefBeacon                :: TxOutRef
    , envValidatorStakeKey        :: BuiltinByteString

    -- Clean utxos check
    , envMinUtxosNumber           :: Int
    , envMaxUtxosNumber           :: Int

    -- Verifier
    , envVerifierPkh              :: BuiltinByteString
    , envVerifierClientEnv        :: ClientEnv

    -- Delegation
    , envDelegationCurrencySymbol :: CurrencySymbol
    , envDelegationTokenName      :: TokenName
    -- ^ We don't get cs and tokenName directly using encoins-core functions
    -- so that we can mock them in tests and quickly get the delegation result.
    , envDelegationSeverHost      :: Text
    , envDelegationServerPort     :: Int
    , envDelegationServerProtocol :: HyperTextProtocol
    , envDelegationIp             :: Text

    -- Logger
    , envLogEnv                   :: LogEnv
    , envKContext                 :: LogContexts
    , envKNamespace               :: Namespace
    , envFormatMessage            :: Bool

    -- Walet
    , envWallet                   :: RestoredWallet
    , envWalletProvider           :: WalletProvider

    -- ChainIndex
    , envChainIndexProvider       :: ChainIndexProvider

    -- Tx
    , envTxProvider               :: TxProvider
    , envCollateral               :: Maybe TxOutRef
    , envLedgerParams             :: Params

    -- Maestro
    , envMaestroToken             :: Maybe MaestroToken

    -- Blockfrost
    , envBlockfrostToken          :: Maybe BlockfrostToken
    }

loadEncoinsRelayEnv :: HasCallStack => EncoinsRelayConfig -> IO EncoinsRelayEnv
loadEncoinsRelayEnv MkEncoinsRelayConfig{..} = do
    envQueueRef  <- newIORef Seq.empty
    envWallet    <- loadWallet cWalletFile
    pp <- decodeOrErrorFromFile cProtocolParametersFile
    slotConfig <- do
        val <- decodeOrErrorFromFile @J.Value cSlotConfigFile
        case val ^? key "cicSlotConfig" <&> J.fromJSON of
            Just (J.Success sc) -> pure sc
            _                   -> error "There is no slot config in chain index config file."
    envDelegationIp <- decodeOrErrorFromFile cDelegationIpFile
    envBlockfrostToken <- sequence $ decodeOrErrorFromFile <$> cBfTokenFilePath
    envMaestroToken    <- sequence $ decodeOrErrorFromFile <$> cMaestroTokenFilePath
    (envWalletProvider, envChainIndexProvider, envTxProvider) <- dataProvidersFromConfigDataProviders cDataProviders
    envLogEnv <- mkLogEnv "Encoins relay server" cEnvironment cVerbosity cSeverity
    envVerifierClientEnv <- mkServantClientEnv cVerifierPort cVerifierHost cVerifierProtocol
    pure MkEncoinsRelayEnv
        { envRefStakeOwner            = cRefStakeOwner
        , envRefBeacon                = cRefBeacon
        , envValidatorStakeKey        = cValidatorStakeKey
        , envVerifierPkh              = cVerifierPkh
        , envDelegationCurrencySymbol = cDelegationCurrencySymbol
        , envDelegationTokenName      = cDelegationTokenName
        , envDelegationSeverHost      = cDelegationServerHost
        , envDelegationServerPort     = cDelegationServerPort
        , envDelegationServerProtocol = cDelegationServerProtocol
        , envKContext                 = mempty
        , envKNamespace               = mempty
        , envFormatMessage            = cFormatMessage
        , envPort                     = cPort
        , envHost                     = cHost
        , envHyperTextProtocol        = cHyperTextProtocol
        , envActiveEndpoints          = cActiveEndpoints
        , envMinUtxosNumber           = cMinUtxosNumber
        , envMaxUtxosNumber           = cMaxUtxosNumber
        , envDiagnosticsInterval      = fromMaybe 300 cDiagnosticsInterval
        , envLedgerParams             = Params slotConfig (pParamsFromProtocolParams pp) cNetworkId
        , envCollateral               = cCollateral
        , ..
        }

instance api ~ EncoinsApiTF => HasSwitchedEndpoints EncoinsServerM api where
    getActiveEndpoints = asks envActiveEndpoints

instance Katip EncoinsServerM where
    getLogEnv = asks envLogEnv
    localLogEnv f (MkEncoinsServerM m) =
        MkEncoinsServerM (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance KatipContext EncoinsServerM where
    getKatipContext = asks envKContext
    localKatipContext f (MkEncoinsServerM m) =
        MkEncoinsServerM (local (\s -> s {envKContext = f (envKContext s)}) m)
    getKatipNamespace = asks envKNamespace
    localKatipNamespace f (MkEncoinsServerM m) =
        MkEncoinsServerM (local (\s -> s {envKNamespace = f (envKNamespace s)}) m)

instance HasNetworkId EncoinsServerM where
    getNetworkId = getNetworkId

instance HasWallet EncoinsServerM where
    getRestoredWallet = asks envWallet

instance HasChainIndexProvider EncoinsServerM where
    getChainIndexProvider = asks envChainIndexProvider

instance MonadMaestro EncoinsServerM where
    getMaestroToken = asks envMaestroToken >>= maybe (throwM NoMaestroToken) pure

instance HasWalletProvider EncoinsServerM where
    getWalletProvider = asks envWalletProvider

instance HasTxProvider EncoinsServerM where
    getTxProvider = asks envTxProvider

instance HasMkTxEnv EncoinsServerM where
    getCollateral   = asks envCollateral
    getLedgerParams = asks envLedgerParams

getTrackedAddresses :: EncoinsServerM [Address]
getTrackedAddresses = do
    encoinsProtocolParams <- getEncoinsProtocolParams
    pure [ledgerValidatorAddress encoinsProtocolParams, alwaysFalseValidatorAddress referenceScriptSalt]

getLedgerAddress :: EncoinsServerM Address
getLedgerAddress = head <$> getTrackedAddresses

getLedgerUtxos :: UtxoRequirements -> EncoinsServerM MapUTXO
getLedgerUtxos reqs = getLedgerAddress >>= getUtxosAt reqs

getEncoinsProtocolParams :: EncoinsServerM EncoinsProtocolParams
getEncoinsProtocolParams = asks (\e -> (envRefStakeOwner e, envRefBeacon e, envVerifierPkh e, envValidatorStakeKey e))

getEncoinsSymbol :: EncoinsServerM CurrencySymbol
getEncoinsSymbol = encoinsSymbol <$> getEncoinsProtocolParams

data EncoinsServerInput
    = InputRedeemer   EncoinsRedeemer EncoinsMode
    | InputSending    Address CSL.Value Address
    | InputDelegation Address Text
    deriving (Show, Generic, FromJSON, ToJSON)

data QueueElem = QueueElem
    { qeInput   :: EncoinsServerInput
    , qeContext :: InputContext
    , qeMvar    :: MVar (Either SomeException ())
    }

newQueueElem :: MonadIO m => EncoinsServerInput -> InputContext -> m QueueElem
newQueueElem qeInput qeContext = do
    qeMvar <- liftIO newEmptyMVar
    pure QueueElem{..}

type Queue = Seq QueueElem

type QueueRef = IORef Queue

data InternalServerError
    = NoWalletProvided
    | NoMaestroToken
    deriving (Show, Exception)
