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

import           Cardano.Server.Config        (decodeOrErrorFromFile)
import           Cardano.Server.Error         (IsCardanoServerError (..))
import           Cardano.Server.Utils.Logger  (HasLogger (..), logMsg, logger, (.<))
import           Control.Exception            (Exception, throw)
import           Control.Monad                (unless)
import           Control.Monad.Catch          (MonadCatch, MonadThrow (..), handle)
import           Control.Monad.Except         (ExceptT (..))
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Aeson                   (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           ENCOINS.BaseTypes            (toGroupElement)
import           ENCOINS.Bulletproofs         (BulletproofSetup, Input (Input), parseBulletproofParams, verify)
import           ENCOINS.Core.OffChain        (mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain         (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           GHC.Generics                 (Generic)
import qualified Network.Wai.Handler.Warp     as Warp
import           PlutusTx.Extra.ByteString    (toBytes)
import           PlutusTx.Prelude             (BuiltinByteString, sha2_256)
import           Servant                      (Handler (Handler), JSON, Proxy (Proxy), ReqBody, StdMethod (GET), UVerb, Union,
                                               WithStatus (..), hoistServer, respond, serve, type (:>))

runVerifierServer :: FilePath -> IO ()
runVerifierServer verifierConfigFp = do
        VerifierConfig{..} <- decodeOrErrorFromFile verifierConfigFp
        bulletproofSetup   <- decodeOrErrorFromFile cBulletproofSetupFilePath
        verifierPrvKey     <- decodeOrErrorFromFile cVerifierPrvKeyFilePath

        unVerifierM $ logMsg "Starting verifier server..."
        Warp.runSettings (mkSettings  VerifierConfig{..}) 
            $ serve (Proxy @VerifierApi) 
            $ hoistServer (Proxy @VerifierApi) 
                (Servant.Handler . ExceptT . fmap Right . unVerifierM) 
                (verifierHandler bulletproofSetup verifierPrvKey)
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
    getLogger = pure logger
    getLoggerFilePath = pure $ Just "verifier.log"

type VerifierApi = "API" :> ReqBody '[JSON] EncoinsRedeemer :> UVerb 'GET '[JSON] VerifierApiResult

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

data VerifierConfig = VerifierConfig
    { cHost                     :: Text
    , cPort                     :: Int
    , cVerifierPkh              :: BuiltinByteString
    , cVerifierPrvKeyFilePath   :: FilePath
    , cBulletproofSetupFilePath :: FilePath
    } deriving (Generic)

instance FromJSON VerifierConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase