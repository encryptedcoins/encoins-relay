{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Verifier.Server where

import           Cardano.Server.Config       (decodeOrErrorFromFile)
import           Cardano.Server.Error        (IsCardanoServerError (..))
import           Cardano.Server.Utils.Logger (HasLogger (..), Logger, logMsg, logger, (.<))
import           Control.Exception           (Exception, throw)
import           Control.Monad               (unless)
import           Control.Monad.Catch         (MonadCatch, MonadThrow (..), handle)
import           Control.Monad.Except        (ExceptT (..))
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT (..), asks)
import           Data.Aeson                  (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing           (aesonPrefix, snakeCase)
import           Data.Maybe                  (fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           ENCOINS.BaseTypes           (toGroupElement)
import           ENCOINS.Bulletproofs        (BulletproofSetup, Input (Input), parseBulletproofParams, verify)
import           ENCOINS.Core.OffChain       (mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain        (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           GHC.Generics                (Generic)
import qualified Network.Wai.Handler.Warp    as Warp
import           PlutusTx.Extra.ByteString   (toBytes)
import           PlutusTx.Prelude            (BuiltinByteString, sha2_256)
import           Servant                     (Get, Handler (Handler), JSON, NoContent (..), Proxy (Proxy), ReqBody,
                                              StdMethod (GET), UVerb, Union, WithStatus (..), hoistServer, respond, serve,
                                              type (:>), (:<|>) ((:<|>)))

runVerifierServer :: FilePath -> IO ()
runVerifierServer verifierConfigFp = do
        VerifierConfig{..} <- decodeOrErrorFromFile verifierConfigFp
        bulletproofSetup   <- decodeOrErrorFromFile cBulletproofSetupFilePath
        verifierPrvKey     <- decodeOrErrorFromFile cVerifierPrvKeyFilePath

        runVerifier $ logMsg "Starting verifier server..."
        Warp.runSettings (mkSettings  VerifierConfig{..})
            $ serve (Proxy @VerifierApi)
            $ hoistServer (Proxy @VerifierApi)
                (Servant.Handler . ExceptT . fmap Right . runVerifier)
                (verifierApi bulletproofSetup verifierPrvKey)
    where
        mkSettings VerifierConfig{..}
            = Warp.setLogger logReceivedRequest
            $ Warp.setOnException (const logException)
            $ Warp.setHost (fromString $ T.unpack cHost)
            $ Warp.setPort cPort Warp.defaultSettings
        logReceivedRequest req status _ = runVerifier $
            logMsg $ "Received request:\n" .< req <> "\nStatus:" .< status
        logException e = runVerifier $
            logMsg $ "Unhandled exception:\n" .< e
        env = VerifierEnv logger (Just "verifier.log")
        runVerifier = (`runReaderT` env) . unVerifierM

newtype VerifierM a = VerifierM {unVerifierM :: ReaderT VerifierEnv IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader VerifierEnv)

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
    , cVerifierPkh              :: BuiltinByteString
    , cVerifierPrvKeyFilePath   :: FilePath
    , cBulletproofSetupFilePath :: FilePath
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