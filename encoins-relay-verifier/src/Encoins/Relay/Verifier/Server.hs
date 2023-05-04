{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Encoins.Relay.Verifier.Server where

import           Cardano.Server.Config        (decodeOrErrorFromFile)
import           Cardano.Server.Error         (IsCardanoServerError (..))
import           Cardano.Server.Utils.Logger  (HasLogger (..), logMsg, (.<))
import qualified Cardano.Server.Utils.Logger  as Logger
import           Control.Exception            (Exception, throw)
import           Control.Monad                (unless)
import           Control.Monad.Catch          (MonadCatch, MonadThrow (..), handle)
import           Control.Monad.Except         (ExceptT (..))
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Aeson                   (FromJSON (..), decode, eitherDecode, genericParseJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.FileEmbed               (embedFile)
import           Data.Maybe                   (fromJust, fromMaybe)
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           ENCOINS.BaseTypes            (toGroupElement)
import           ENCOINS.Bulletproofs         (BulletproofSetup, Input (Input), parseBulletproofParams, verify)
import           ENCOINS.Core.OnChain         (EncoinsRedeemer, hashRedeemer)
import           GHC.Generics                 (Generic)
import           GHC.Stack                    (HasCallStack)
import qualified Network.Wai.Handler.Warp     as Warp
import           PlutusAppsExtra.Utils.Crypto (sign)
import           PlutusTx.Extra.ByteString    (toBytes)
import           PlutusTx.Prelude             (BuiltinByteString, sha2_256)
import           Servant                      (Handler (Handler), JSON, Proxy (Proxy), ReqBody, StdMethod (GET), UVerb, Union,
                                               WithStatus (..), hoistServer, respond, serve, type (:>))

runVerifierServer :: IO ()
runVerifierServer = do
        c <- loadVerifierConfig
        unVerifierM $ logMsg "Starting verifier server..."
        Warp.runSettings (mkSettings c) 
            $ serve (Proxy @VerifierApi) 
            $ hoistServer (Proxy @VerifierApi) (Servant.Handler . ExceptT . fmap Right . unVerifierM) verifierHandler
    where
        mkSettings VerifierConfig{..} 
            = Warp.setLogger logReceivedRequest
            $ Warp.setOnException (const logException)
            $ Warp.setHost (fromString $ T.unpack cHost)
            $ Warp.setPort cPort Warp.defaultSettings
        logReceivedRequest req status _ = unVerifierM $
            logMsg $ "Received request:\n" .< req <> "\nStatus:" .< status
        logException e = unVerifierM $
            logMsg $ "Unhandled exception:\n" .< e

newtype VerifierM a = VerifierM {unVerifierM :: IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance HasLogger VerifierM where
    getLogger = pure Logger.logger
    getLoggerFilePath = pure $ Just $ Logger.mkFullPath "verifier"

type VerifierApi = "API" :> ReqBody '[JSON] EncoinsRedeemer :> UVerb 'GET '[JSON] VerifierApiResult

type VerifierApiResult = '[WithStatus 200 EncoinsRedeemer, WithStatus 422 Text]

verifierHandler :: EncoinsRedeemer -> VerifierM (Union VerifierApiResult)
verifierHandler red@(par, input, proof, _) = handle errHandler $ do
    let bp   = parseBulletproofParams $ sha2_256 $ toBytes par
        v    = fst input
        ins  = map (\(bs, p) -> Input (fromMaybe (throw IncorrectInput) $ toGroupElement bs) p) $ snd input
    unless (verify bulletproofSetup bp v ins proof) $ throwM IncorrectProof
    respond $ WithStatus @200 (par, input, proof, sign verifierPrvKey $ hashRedeemer red)
    where
        errHandler :: VerifierApiError -> VerifierM (Union VerifierApiResult)
        errHandler = respond . WithStatus @422 . errMsg

data VerifierApiError
    = IncorrectInput
    | IncorrectProof
    deriving (Show, Exception)

instance IsCardanoServerError VerifierApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        IncorrectInput -> "The request contained incorrect public input."
        IncorrectProof -> "The request contained incorrect proof."

data VerifierConfig = VerifierConfig
    { cHost :: Text
    , cPort :: Int
    } deriving (Generic)

instance FromJSON VerifierConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

loadVerifierConfig :: HasCallStack => IO VerifierConfig
loadVerifierConfig = decodeOrErrorFromFile "verifierConfig.json"

bulletproofSetup :: BulletproofSetup
bulletproofSetup = either error id $ eitherDecode $ fromStrict $(embedFile "../config/bulletproof_setup.json")

verifierPrvKey :: BuiltinByteString
verifierPrvKey = fromJust $ decode $ fromStrict $(embedFile "../config/prvKey.json")