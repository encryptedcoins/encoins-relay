{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Verifier.Server where

import           Cardano.Server.Config        (HyperTextProtocol (..), decodeOrErrorFromFile)
import           Cardano.Server.Error         (IsCardanoServerError (..))
import           Control.Exception            (Exception, throw)
import           Control.Monad                (unless)
import           Control.Monad.Catch          (MonadCatch, MonadThrow (..), handle)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Reader         (MonadReader (..), ReaderT (..), asks)
import           Data.Aeson                   (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import           Development.GitRev           (gitCommitDate, gitHash)
import           ENCOINS.BaseTypes            (toGroupElement)
import           ENCOINS.Bulletproofs         (BulletproofSetup, Input (Input), parseBulletproofParams, verify)
import           ENCOINS.Core.OffChain        (mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain         (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           Encoins.Common.Log           (logInfo, mkLogEnv, withLogEnv)
import           Encoins.Common.Version       (appVersion, showAppVersion)
import           Encoins.Relay.Common.Server  (runCardanoServer)
import           GHC.Generics                 (Generic)
import           Katip                        (Environment, Katip (..), KatipContext (..), LogContexts, LogEnv, Namespace, Severity,
                                               Verbosity)
import           Paths_encoins_relay_verifier (version)
import           PlutusTx.Extra.ByteString    (toBytes)
import           PlutusTx.Prelude             (BuiltinByteString, sha2_256)
import           Say                          (say)
import           Servant                      (Get, JSON, NoContent (..), ReqBody, ServerError, StdMethod (GET), UVerb, Union,
                                               WithStatus (..), respond, type (:>), (:<|>) ((:<|>)))
import qualified Servant

runVerifierServer :: FilePath -> IO ()
runVerifierServer verifierConfigFp = do
    MkVerifierConfig{..} <- decodeOrErrorFromFile verifierConfigFp
    bulletproofSetup <- decodeOrErrorFromFile cBulletproofSetupFilePath
    verifierPrvKey <- decodeOrErrorFromFile cVerifierPrvKeyFilePath
    let logEnv = mkLogEnv
            "Verifier server"
            cEnvironment
            cVerbosity
            cSeverity
    withLogEnv logEnv $ \le -> do
        let env = MkVerifierEnv le mempty mempty cFormatMessage
        runCardanoServer @VerifierApi
            cPort
            ((`runReaderT` env) . unVerifierM)
            (verifierApi bulletproofSetup verifierPrvKey)
            beforeMainLoop
  where
    beforeMainLoop = say $ showAppVersion "Verifier server" $ appVersion
        version
        $(gitHash)
        $(gitCommitDate)

newtype VerifierM a = MkVerifierM {unVerifierM :: ReaderT VerifierEnv Servant.Handler a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader VerifierEnv
        , MonadError Servant.ServerError
        )

instance Katip VerifierM where
    getLogEnv = asks vEnvLogEnv
    localLogEnv f (MkVerifierM m) =
        MkVerifierM (local (\s -> s {vEnvLogEnv = f (vEnvLogEnv s)}) m)

instance KatipContext VerifierM where
    getKatipContext = asks vEnvKContext
    localKatipContext f (MkVerifierM m) =
        MkVerifierM (local (\s -> s {vEnvKContext = f (vEnvKContext s)}) m)
    getKatipNamespace = asks vEnvKNamespace
    localKatipNamespace f (MkVerifierM m) =
        MkVerifierM (local (\s -> s {vEnvKNamespace = f (vEnvKNamespace s)}) m)

data VerifierEnv = MkVerifierEnv
    { vEnvLogEnv        :: LogEnv
    , vEnvKContext      :: LogContexts
    , vEnvKNamespace    :: Namespace
    , vEnvFormatMessage :: Bool
    }

-- Format of severity in json file:
-- debug, info, notice, warning, error, critical, alert, emergency
-- Format of verbosity in json file: V0, V1, V2, V3
data VerifierConfig = MkVerifierConfig
    { cHost                     :: Text
    , cPort                     :: Int
    , cHyperTextProtocol        :: HyperTextProtocol
    , cVerifierPkh              :: BuiltinByteString
    , cVerifierPrvKeyFilePath   :: FilePath
    , cBulletproofSetupFilePath :: FilePath
    , cEnvironment              :: Environment
    , cVerbosity                :: Verbosity
    , cSeverity                 :: Severity
    , cFormatMessage            :: Bool
    } deriving (Generic)

instance FromJSON VerifierConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------------------------------------------------- API --------------------------------------------------

type VerifierApi = PingEnpoint :<|> VerifyEndpoint

verifierApi :: BulletproofSetup -> BuiltinByteString -> VerifierM NoContent :<|> (EncoinsRedeemer -> VerifierM (Union VerifierApiResult))
verifierApi bulletproofSetup verifierPrvKey = pingHandler :<|> verifierHandler bulletproofSetup verifierPrvKey

---------------------------------------------- Ping endpoint ----------------------------------------------

type PingEnpoint = "ping" :> Get '[JSON] NoContent

pingHandler :: VerifierM NoContent
pingHandler = do
    logInfo "Received ping request."
    pure NoContent

--------------------------------------------- Verify endpoint ---------------------------------------------

type VerifyEndpoint = "API" :> ReqBody '[JSON] EncoinsRedeemer :> UVerb 'GET '[JSON] VerifierApiResult

type VerifierApiResult = '[WithStatus 200 EncoinsRedeemerOnChain, WithStatus 422 Text]

verifierHandler :: BulletproofSetup -> BuiltinByteString -> EncoinsRedeemer -> VerifierM (Union VerifierApiResult)
verifierHandler bulletproofSetup verifierPrvKey red@(par, input, proof, _) = handle errHandler $ do
    let bp   = parseBulletproofParams $ sha2_256 $ toBytes par
        v    = fst input
        ins  = map (\(bs, p) -> Input (fromMaybe (throw IncorrectInput) $ toGroupElement bs) p) $ snd input
    unless (verify bulletproofSetup bp v ins proof) $ throwM IncorrectProof
    respond $ WithStatus @200 $ mkEncoinsRedeemerOnChain verifierPrvKey red
    where
        errHandler :: VerifierApiError -> VerifierM (Union VerifierApiResult)
        errHandler = respond . WithStatus @422 . errMsg

data VerifierApiError
    = IncorrectInput
    | IncorrectProof
    deriving (Show, Exception, Eq)

instance IsCardanoServerError VerifierApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        IncorrectInput -> "The request contained incorrect public input."
        IncorrectProof -> "The request contained incorrect proof."
