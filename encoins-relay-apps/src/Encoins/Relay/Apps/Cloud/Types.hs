{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Encoins.Relay.Apps.Cloud.Types where

import           Cardano.Api                   (NetworkId, NetworkMagic)
import           Cardano.Server.Config         (HyperTextProtocol (..))
import           Control.Exception.Safe        (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, ReaderT (..), asks,
                                                local)
import           Data.Aeson                    (FromJSON (..), FromJSONKey,
                                                ToJSON (..), ToJSONKey,
                                                defaultOptions,
                                                genericParseJSON, genericToJSON,
                                                tagSingleConstructors)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Text                     (Text)
import           Data.Time.Clock               (NominalDiffTime)
import           Dhall                         (FromDhall (..))
import           GHC.Generics                  (Generic)
import qualified Hasql.Pool                    as P
import           Katip
import           Network.HTTP.Client           (Manager)
import           Numeric.Natural               (Natural)
import           Plutus.V1.Ledger.Api          (CurrencySymbol)
import           PlutusAppsExtra.Api.Maestro   (MaestroToken, MonadMaestro (..))
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           PlutusTx.Builtins.Internal    (BuiltinByteString (BuiltinByteString))
import           Servant.API                   (ToHttpApiData)


-- General types

data CloudEnv = MkCloudEnv
  { envHyperTextProtocol :: HyperTextProtocol
  , envHost              :: Text
  , envPort              :: Int
  , envNetworkId         :: NetworkId
  , envMaestroToken      :: MaestroToken
  , envCurrencySymbol    :: CurrencySymbol
  , envManager           :: Manager
  , envLogEnv            :: LogEnv
  , envKContext          :: LogContexts
  , envKNamespace        :: Namespace
  , envFormatMessage     :: Bool -- Pretty print message or not
  , envPool              :: P.Pool
  , envDiscardPeriod     :: NominalDiffTime -- seconds 12*60*60
  , envCleanDelay        :: NominalDiffTime -- seconds 7*24*60*60
  }

-- Format of severity in json file:
-- debug, info, notice, warning, error, critical, alert, emergency
-- Format of verbosity in json file: V0, V1, V2, V3
data CloudConfig = MkCloudConfig
  { icHyperTextProtocol    :: HyperTextProtocol
  , icHost                 :: Text
  , icPort                 :: Int
  , icNetworkId            :: NetworkId
  , icMaestroTokenFilePath :: FilePath
  , icCurrencySymbol       :: CurrencySymbol
  , icEnvironment          :: Environment
  , icVerbosity            :: Verbosity
  , icSeverity             :: Severity
  , icFormatMessage        :: Bool
  , icDiscardPeriod        :: NominalDiffTime -- seconds
  , icCleanDelay           :: NominalDiffTime -- seconds
  }
  deriving stock (Eq,Show, Generic)
  deriving anyclass (FromDhall)

deriving anyclass instance FromDhall HyperTextProtocol
deriving anyclass instance FromDhall NetworkMagic
deriving anyclass instance FromDhall NetworkId
deriving anyclass instance FromDhall CurrencySymbol
deriving anyclass instance FromDhall Environment
deriving anyclass instance FromDhall Verbosity
deriving anyclass instance FromDhall Severity

instance FromDhall NominalDiffTime where
    autoWith opts = fmap (fromInteger . toInteger @Natural) (autoWith opts)

instance FromDhall BuiltinByteString where
    autoWith opts = fmap BuiltinByteString (autoWith opts)

instance FromJSON CloudConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype CloudMonad a = MkCloudMonad {getCloudMonad :: ReaderT CloudEnv IO a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadFail
        , MonadCatch
        , MonadReader CloudEnv
        )

runCloudMonad :: CloudEnv -> CloudMonad a -> IO a
runCloudMonad env = (`runReaderT` env) . getCloudMonad

instance HasNetworkId CloudMonad where
  getNetworkId = asks envNetworkId

instance MonadMaestro CloudMonad where
  getMaestroToken = asks envMaestroToken

instance Katip CloudMonad where
  getLogEnv = asks envLogEnv
  localLogEnv f (MkCloudMonad m) =
    MkCloudMonad (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance KatipContext CloudMonad where
  getKatipContext = asks envKContext
  localKatipContext f (MkCloudMonad m) =
    MkCloudMonad (local (\s -> s {envKContext = f (envKContext s)}) m)
  getKatipNamespace = asks envKNamespace
  localKatipNamespace f (MkCloudMonad m) =
    MkCloudMonad (local (\s -> s {envKNamespace = f (envKNamespace s)}) m)

-- Backend Request/Response types

-- EncryptedSecret is encoded as Text
newtype EncryptedSecret = MkEncryptedSecret { getEncryptedSecret :: Text }
  deriving newtype (Eq, Show, ToJSON, FromJSON)
  deriving stock (Generic)

newtype AssetName = MkAssetName { getAssetName :: Text }
  deriving newtype (Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, ToHttpApiData)
  deriving stock (Generic)

-- Request body from frontend to backend
data SaveRequest = MkSaveRequest
  { ppAssetName :: AssetName
  , ppSecretKey :: EncryptedSecret
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SaveRequest where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- Used to tag maestro status
data CoinStatus = CoinMinted | CoinBurned | CoinDiscarded Text | CoinError Text
  deriving stock (Eq, Show)

-- Used to response to the client
data SaveStatus = Saved | SaveError | Discarded
  deriving stock (Show, Eq, Generic)

instance ToJSON SaveStatus where
  toJSON = genericToJSON $
    defaultOptions{tagSingleConstructors = True}

data StatusResponse = MkStatusResponse
  { spStatusResponse :: SaveStatus
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON StatusResponse where
   toJSON = genericToJSON $ aesonPrefix snakeCase

data SaveToken = MkSaveToken
  { stAssetName :: AssetName
  , stSecret    :: EncryptedSecret
  , stDuplicate :: Natural
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SaveToken where
   toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SaveToken where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
