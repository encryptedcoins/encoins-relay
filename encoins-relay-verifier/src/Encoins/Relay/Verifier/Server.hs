{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Encoins.Relay.Verifier.Server where

import           Cardano.Server.Config            (Creds, decodeOrErrorFromFile, Config (..), AuxillaryConfigOf)
import           Cardano.Server.EndpointName      (EndpointWithName)
import           Cardano.Server.Endpoints.Ping    (PingApi, pingHandler)
import           Cardano.Server.Endpoints.Version (VersionApi, versionEndpointHandler)
import           Cardano.Server.Error             (IsCardanoServerError (..), Throws)
import           Cardano.Server.Handler           (wrapHandler)
import           Cardano.Server.Internal          (AuxillaryEnvOf, ServerM, loadEnv)
import           Cardano.Server.Main              (runServer, RunSettings (..))
import           Control.Exception                (Exception (..), throw)
import           Control.Monad                    (unless)
import           Control.Monad.Catch              (MonadThrow (..))
import           Data.Aeson                       (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing                (aesonPrefix, snakeCase)
import           Data.Default                     (Default (..))
import           Data.FileEmbed                   (embedFileIfExists)
import           Data.Maybe                       (fromMaybe)
import           Development.GitRev               (gitCommitDate, gitHash)
import           ENCOINS.BaseTypes                (toGroupElement)
import           ENCOINS.Bulletproofs             (BulletproofSetup, Input (Input), parseBulletproofParams, verify)
import           ENCOINS.Core.OffChain            (mkEncoinsRedeemerOnChain)
import           ENCOINS.Core.OnChain             (EncoinsRedeemer, EncoinsRedeemerOnChain)
import           GHC.Generics                     (Generic)
import           Paths_encoins_relay_verifier     (version)
import           PlutusTx.Extra.ByteString        (toBytes)
import           PlutusTx.Prelude                 (BuiltinByteString, sha2_256)
import           Servant                          (Get, JSON, ReqBody, type (:>), (:<|>) ((:<|>)))
import qualified Servant

runVerifierServer :: FilePath -> IO ()
runVerifierServer verifierConfigFp = do
    c@Config {cAuxilaryConfig = VerifierConfig {..}} <- decodeOrErrorFromFile verifierConfigFp
    bulletproofSetup   <- decodeOrErrorFromFile cBulletproofSetupFilePath
    verifierPrvKey     <- decodeOrErrorFromFile cVerifierPrvKeyFilePath
    let ?creds = creds
    env <- loadEnv c ()
    runServer @VerifierApi (verifierApi bulletproofSetup verifierPrvKey) env def
        { rsServerName = "verifier server"
        }

creds :: Creds
creds = let keyCred  = $(embedFileIfExists "../key.pem")
            certCred = $(embedFileIfExists "../certificate.pem")
        in (,) <$> certCred <*> keyCred

type instance AuxillaryEnvOf    VerifierApi = ()
type instance AuxillaryConfigOf VerifierApi = VerifierConfig

data VerifierConfig = VerifierConfig
    { cVerifierPkh              :: BuiltinByteString
    , cVerifierPrvKeyFilePath   :: FilePath
    , cBulletproofSetupFilePath :: FilePath
    } deriving (Generic)

instance FromJSON VerifierConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------------------------------------------------- API --------------------------------------------------

type VerifierApi
    =    PingApi
    :<|> VersionApi
    :<|> VerifyEndpoint

verifierApi :: BulletproofSetup -> BuiltinByteString -> Servant.ServerT VerifierApi (ServerM VerifierApi)
verifierApi bulletproofSetup verifierPrvKey
    =   pingHandler
    :<|> versionEndpointHandler version $(gitHash) $(gitCommitDate)
    :<|> verifierHandler bulletproofSetup verifierPrvKey

--------------------------------------------- Verify endpoint ---------------------------------------------

type VerifyEndpoint = "API"
    :> Throws VerifierApiError
    :> ReqBody '[JSON] EncoinsRedeemer
    :> Get '[JSON] EncoinsRedeemerOnChain

verifierHandler :: BulletproofSetup -> BuiltinByteString -> EncoinsRedeemer -> ServerM VerifierApi EncoinsRedeemerOnChain
verifierHandler bulletproofSetup verifierPrvKey red@(par, input, proof, _) =
    wrapHandler @(EndpointWithName "verify" VerifyEndpoint) $ do
    let bp   = parseBulletproofParams $ sha2_256 $ toBytes par
        v    = fst input
        ins  = map (\(bs, p) -> Input (fromMaybe (throw IncorrectInput) $ toGroupElement bs) p) $ snd input
    unless (verify bulletproofSetup bp v ins proof) $ throwM IncorrectProof
    pure $ mkEncoinsRedeemerOnChain verifierPrvKey red

data VerifierApiError
    = IncorrectInput
    | IncorrectProof
    deriving (Show, Exception, Eq)

instance IsCardanoServerError VerifierApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        IncorrectInput -> "The request contained incorrect public input."
        IncorrectProof -> "The request contained incorrect proof."
    restore 422 = \case
        "The request contained incorrect public input." -> Just IncorrectInput
        "The request contained incorrect proof."        -> Just IncorrectProof
        _                                               -> Nothing
    restore _ = const Nothing