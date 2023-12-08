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

import           Cardano.Server.Config       (CardanoServerConfig (..), HyperTextProtocol (..), decodeOrErrorFromFile)
import           Cardano.Server.Error        (IsCardanoServerError (..))
import           Cardano.Server.Main         (runCardanoServer)
import           Cardano.Server.Utils.Logger (HasLogger (..), Logger, logMsg, logger)
import           Control.Exception           (Exception, throw)
import           Control.Monad               (unless)
import           Control.Monad.Catch         (MonadCatch, MonadThrow (..), handle)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT (..), asks)
import           Data.Aeson                  (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing           (aesonPrefix, snakeCase)
import           Data.ByteString             (ByteString)
import           Data.FileEmbed              (embedFileIfExists)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           ENCOINS.BaseTypes           (toGroupElement)
import           ENCOINS.Bulletproofs        (BulletproofSetup, Input (Input), parseBulletproofParams, verify)
import           ENCOINS.Core.OffChain       (mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain        (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           GHC.Generics                (Generic)
import           PlutusTx.Extra.ByteString   (toBytes)
import           PlutusTx.Prelude            (BuiltinByteString, sha2_256)
import           Servant                     (Get, JSON, NoContent (..), ReqBody, ServerError, StdMethod (GET), UVerb, Union,
                                              WithStatus (..), respond, type (:>), (:<|>) ((:<|>)))
import qualified Servant

runVerifierServer :: FilePath -> IO ()
runVerifierServer verifierConfigFp = do
        VerifierConfig{..} <- decodeOrErrorFromFile verifierConfigFp
        bulletproofSetup   <- decodeOrErrorFromFile cBulletproofSetupFilePath
        verifierPrvKey     <- decodeOrErrorFromFile cVerifierPrvKeyFilePath
        let ?creds = creds
        runCardanoServer @VerifierApi
            VerifierConfig{..}
            ((`runReaderT` env) . unVerifierM)
            (verifierApi bulletproofSetup verifierPrvKey)
            beforeMainLoop
    where
        env = VerifierEnv logger (Just "verifier.log")
        beforeMainLoop = logMsg "Starting verifier server..."

creds :: Maybe (ByteString, ByteString)
creds = let keyCred  = $(embedFileIfExists "../key.pem")
            certCred = $(embedFileIfExists "../certificate.pem")
        in (,) <$> certCred <*> keyCred

newtype VerifierM a = VerifierM {unVerifierM :: ReaderT VerifierEnv Servant.Handler a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader VerifierEnv, MonadError Servant.ServerError)

instance HasLogger VerifierM where
    getLogger = asks vEnvLogger
    getLoggerFilePath = asks vEnvLoggerFp

data VerifierEnv = VerifierEnv
    { vEnvLogger   :: Logger VerifierM
    , vEnvLoggerFp :: Maybe FilePath
    }

data VerifierConfig = VerifierConfig
    { cHost                     :: Text
    , cPort                     :: Int
    , cHyperTextProtocol        :: HyperTextProtocol
    , cVerifierPkh              :: BuiltinByteString
    , cVerifierPrvKeyFilePath   :: FilePath
    , cBulletproofSetupFilePath :: FilePath
    } deriving (Generic)

instance FromJSON VerifierConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance CardanoServerConfig VerifierConfig where
    configHost              = cHost
    configPort              = cPort
    configHyperTextProtocol = cHyperTextProtocol

-------------------------------------------------- API --------------------------------------------------

type VerifierApi = PingEnpoint :<|> VerifyEndpoint

verifierApi :: BulletproofSetup -> BuiltinByteString -> VerifierM NoContent :<|> (EncoinsRedeemer -> VerifierM (Union VerifierApiResult))
verifierApi bulletproofSetup verifierPrvKey = pingHandler :<|> verifierHandler bulletproofSetup verifierPrvKey

---------------------------------------------- Ping endpoint ----------------------------------------------

type PingEnpoint = "ping" :> Get '[JSON] NoContent

pingHandler :: VerifierM NoContent
pingHandler = NoContent <$ logMsg "Received ping request."

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