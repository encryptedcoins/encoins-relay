{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Apps.Save.Types where

import           Encoins.Common.Transform      (toJsonText)

import           Cardano.Api                   (NetworkId)
import           Cardano.Server.Config         (HyperTextProtocol (..))
import           Control.Exception.Safe        (Exception, MonadCatch,
                                                MonadThrow)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, ReaderT (..), asks,
                                                local)
import           Data.Aeson                    (FromJSON (..), FromJSONKey,
                                                Options (fieldLabelModifier),
                                                SumEncoding (..), ToJSON (..),
                                                ToJSONKey, camelTo2,
                                                constructorTagModifier,
                                                defaultOptions,
                                                genericParseJSON, genericToJSON,
                                                sumEncoding,
                                                tagSingleConstructors,
                                                withObject, (.:), (.:?))
import           Data.Aeson.Casing             (aesonPrefix, camelCase,
                                                snakeCase)
import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import           Data.Time.Clock.POSIX         (POSIXTime)
import           GHC.Generics                  (Generic)
import           Katip
import           Network.HTTP.Client           (Manager)
import           Numeric.Natural               (Natural)
import           Plutus.V1.Ledger.Api          (CurrencySymbol)
import           PlutusAppsExtra.Api.Maestro   (MaestroToken, MonadMaestro (..))
import           PlutusAppsExtra.Utils.Network (HasNetworkId (..))
import           Servant.API                   (ToHttpApiData)
import           Servant.Client                (ClientError)
import           Data.Semigroup                 (Max (..))

-- General types

data SaveEnv = MkIpfsEnv
  { envHyperTextProtocol :: HyperTextProtocol
  , envHost              :: Text
  , envPort              :: Int
  , envNetworkId         :: NetworkId
  , envMaestroToken      :: MaestroToken
  , envCurrencySymbol    :: CurrencySymbol
  , envSaveDirectory     :: FilePath
  , envManager           :: Manager
  , envLogEnv            :: LogEnv
  , envKContext          :: LogContexts
  , envKNamespace        :: Namespace
  , envFormatMessage     :: Bool -- Pretty print message or not
  }

-- Format of severity in json file:
-- debug, info, notice, warning, error, critical, alert, emergency
-- Format of verbosity in json file: V0, V1, V2, V3
data SaveConfig = MkSaveConfig
  {
    icHyperTextProtocol    :: HyperTextProtocol
  , icHost                 :: Text
  , icPort                 :: Int
  , icNetworkId            :: NetworkId
  , icMaestroTokenFilePath :: FilePath
  , icCurrencySymbol       :: CurrencySymbol
  , icSaveDirectory        :: FilePath
  , icEnvironment          :: Environment
  , icVerbosity            :: Verbosity
  , icSeverity             :: Severity
  , icFormatMessage        :: Bool
  }
  deriving stock (Eq,Show, Generic)

instance FromJSON SaveConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype SaveMonad a = MkSaveMonad {getSaveMonad :: ReaderT SaveEnv IO a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader SaveEnv
        )

runSaveMonad :: SaveEnv -> SaveMonad a -> IO a
runSaveMonad env = (`runReaderT` env) . getSaveMonad

instance HasNetworkId SaveMonad where
  getNetworkId = asks envNetworkId

instance MonadMaestro SaveMonad where
  getMaestroToken = asks envMaestroToken

instance Katip SaveMonad where
  getLogEnv = asks envLogEnv
  localLogEnv f (MkSaveMonad m) =
    MkSaveMonad (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance KatipContext SaveMonad where
  getKatipContext = asks envKContext
  localKatipContext f (MkSaveMonad m) =
    MkSaveMonad (local (\s -> s {envKContext = f (envKContext s)}) m)
  getKatipNamespace = asks envKNamespace
  localKatipNamespace f (MkSaveMonad m) =
    MkSaveMonad (local (\s -> s {envKNamespace = f (envKNamespace s)}) m)

-- Backend Request/Response types

-- EncryptedToken and EncryptedSecret are semantically the same.
-- The difference is in their JSON instances.
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

-- data RottenToken = MkRottenToken
--   { rtAssetName  :: AssetName
--   , rtRemoveTime :: POSIXTime
--   , rtCip        :: Cip
--   }
--   deriving stock (Show, Eq, Generic)

-- instance FromJSON RottenToken where
--    parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- instance ToJSON RottenToken where
--    toJSON = genericToJSON $ aesonPrefix snakeCase

-- data RestoreResponse = MkRestoreResponse
--   { rrAssetName       :: AssetName
--   , rrEncryptedSecret :: EncryptedSecret
--   }
--   deriving stock (Show, Eq, Generic)

-- instance ToJSON RestoreResponse where
--    toJSON = genericToJSON $ aesonPrefix snakeCase

-- data RestoreError = Client ClientError | InvalidStatus CoinStatus
--   deriving stock (Show, Eq)
