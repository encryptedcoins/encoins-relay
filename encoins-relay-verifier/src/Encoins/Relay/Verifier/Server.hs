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

import           Control.Applicative                       ((<|>))
import           Control.Exception                         (Exception, throw)
import           Control.Monad                             (unless)
import           Control.Monad.Catch                       (MonadCatch, MonadThrow (..), handle)
import           Control.Monad.Except                      (MonadError)
import           Control.Monad.IO.Class                    (MonadIO)
import           Control.Monad.Reader                      (MonadReader (..), ReaderT (..), asks)
import           Data.Aeson                                (FromJSON (..), ToJSON (..), genericParseJSON, (.:), (.=))
import qualified Data.Aeson                                as J
import           Data.Aeson.Casing                         (aesonPrefix, snakeCase)
import           Data.Coerce                               (coerce)
import           Data.Maybe                                (fromMaybe)
import           Data.Text                                 (Text)
import           Development.GitRev                        (gitCommitDate, gitHash)
import           ENCOINS.BaseTypes                         (toGroupElement)
import           ENCOINS.Bulletproofs                      (BulletproofSetup, Input (Input), Proof, parseBulletproofParams, verify)
import           Encoins.Common.Log                        (mkLogEnv, withLogEnv)
import           Encoins.Common.Version                    (appVersion, showAppVersion)
import           ENCOINS.Core.OffChain                     (mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain                      (EncoinsInput, EncoinsInputOnChain, EncoinsRedeemer, EncoinsRedeemerOnChain,
                                                            ProofHash, ProofSignature, TxParams)
import           Encoins.Relay.Common                      (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Error                (IsCardanoServerError (..))
import           Encoins.Relay.Common.Server               (HyperTextProtocol, runCardanoServer)
import           Encoins.Relay.Common.Server.Endpoint.Ping (PingEndpoint, pingEndpointHandler)
import           GHC.Generics                              (Generic)
import           Katip                                     (Environment, Katip (..), KatipContext (..), LogContexts, LogEnv, Namespace,
                                                            Severity, Verbosity)
import           Ledger                                    (Address (..), ScriptHash (..))
import           Paths_encoins_relay_verifier              (version)
import           PlutusAppsExtra.Utils.Orphans             ()
import           PlutusLedgerApi.V1.Credential             (Credential (..), StakingCredential (..))
import           PlutusTx.Extra.ByteString                 (ToBuiltinByteString, toBytes)
import           PlutusTx.Prelude                          (BuiltinByteString, sha2_256)
import           Say                                       (say)
import qualified Servant
import           Servant                                   (JSON, NoContent (..), ReqBody, ServerError, StdMethod (GET), UVerb, Union,
                                                            WithStatus (..), respond, type (:>), (:<|>) ((:<|>)))

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

type VerifierApi = PingEndpoint :<|> VerifyEndpoint

verifierApi :: BulletproofSetup -> BuiltinByteString
    ->   VerifierM NoContent
    :<|> (LegacyEncoinsRedeemer -> VerifierM (Union VerifierApiResult))
verifierApi bulletproofSetup verifierPrvKey = pingEndpointHandler :<|> verifierHandler bulletproofSetup verifierPrvKey

--------------------------------------------- Verify endpoint ---------------------------------------------

type VerifyEndpoint = "API" :> ReqBody '[JSON] LegacyEncoinsRedeemer :> UVerb 'GET '[JSON] VerifierApiResult

type VerifierApiResult = '[WithStatus 200 LegacyEncoinsRedeemerOnChain, WithStatus 422 Text]

verifierHandler :: BulletproofSetup -> BuiltinByteString -> LegacyEncoinsRedeemer -> VerifierM (Union VerifierApiResult)
verifierHandler bulletproofSetup verifierPrvKey red@(par, input, proof, _) = handle errHandler $ do
    let bp   = parseBulletproofParams $ sha2_256 $ toBytes $ coerce @_ @TxParams par
        v    = fst input
        ins  = map (\(bs, p) -> Input (fromMaybe (throw IncorrectInput) $ toGroupElement bs) p) $ snd input
    unless (verify bulletproofSetup bp v ins proof) $ throwM IncorrectProof
    respond $ WithStatus @200 $ coerce @_ @LegacyEncoinsRedeemerOnChain $ mkEncoinsRedeemerOnChain verifierPrvKey $ coerce red
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


-- | JSON encoding of the address changes in one of the plutus updates. This newtype keeps previous encoding.
newtype LegacyAddress = MkLegacyAddress Address

instance ToJSON LegacyAddress where
    toJSON (MkLegacyAddress (Address cred mbSCred)) =
        J.object [ "addressCredential"        .= cred'
                 , "addressStakingCredential" .= toJSON mbSCred
                 ]
      where
        cred' = case cred of
                ScriptCredential (ScriptHash sh) -> J.object
                    [ "contents" .= sh
                    , "tag" .= ("ScriptCredential" :: Text)
                    ]
                pkCred                           -> toJSON pkCred

instance FromJSON LegacyAddress where
    parseJSON = J.withObject "LegacyAddress" $ \o -> do
            cred    <- o .: "addressCredential" <|>  (o .: "addressCredential" >>= parseCred)
            mbSCred <- o .: "addressStakingCredential"
            pure $ MkLegacyAddress $ Address cred mbSCred
      where
        parseCred = J.withObject "LegacyCredential" $ \c -> do
            J.String "ScriptCredential" <- c .: "tag"
            sh <- c .: "contents"
            pure $ ScriptCredential $ ScriptHash sh

-- | JSON encoding of the address changes in one of the plutus updates. This type keeps previous encoding.
type LegacyTxParams = (LegacyAddress, LegacyAddress, Integer)

-- | JSON encoding of the address changes in one of the plutus updates. This type keeps previous encoding.
type LegacyEncoinsRedeemer = (LegacyTxParams, EncoinsInput, Proof, ProofSignature)

-- | JSON encoding of the address changes in one of the plutus updates. This type keeps previous encoding.
type LegacyEncoinsRedeemerOnChain = (LegacyTxParams, EncoinsInputOnChain, ProofHash, ProofSignature)

-- addressExample = Address
--         { addressCredential = ScriptCredential "09cc78edf1b8cd8243613de8d3ee6d6c8752e60241fe02929b5e8685"
--         , addressStakingCredential = Just
--             ( StakingHash
--                 ( PubKeyCredential ""))
--         }

