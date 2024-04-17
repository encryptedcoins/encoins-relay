{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Encoins.Relay.Server.Config where

import           Cardano.Api                   (writeFileJSON)
import           Cardano.Server.Config         (Config (..), HyperTextProtocol, addMissingDirectories, decodeOrErrorFromFile)
import           Control.Monad.Extra           (unlessM)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Aeson                    (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing             (aesonPrefix, snakeCase)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Plutus.V2.Ledger.Api          (Address, CurrencySymbol, TokenName, TxOutRef (..))
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusTx.Builtins             (BuiltinByteString)
import           System.Directory              (doesFileExist)

loadEncoinsRelayConfig :: MonadIO m => Config -> m EncoinsRelayConfig
loadEncoinsRelayConfig c = liftIO $ decodeOrErrorFromFile $ cAuxiliaryEnvFile c

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr1q8u2rh5uud6yzmhq0de7vt7p0rvqpfadwnee3tjnz2tl4rct6qt03wjc2lfwyqnd54gwfdey50s7342e3jl6kxwww4kqzfah2x"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

data EncoinsRelayConfig = EncoinsRelayConfig
    -- Relay
    { cRefStakeOwner            :: TxOutRef
    , cRefBeacon                :: TxOutRef
    , cValidatorStakeKey        :: BuiltinByteString
    -- Verifier
    , cVerifierPkh              :: BuiltinByteString
    , cVerifierHost             :: Text
    , cVerifierPort             :: Int
    , cVerifierProtocol         :: HyperTextProtocol
    -- Delegation
    , cDelegationCurrencySymbol :: CurrencySymbol
    , cDelegationTokenName      :: TokenName
    , cDelegationServerHost     :: Text
    , cDelegationServerPort     :: Int
    , cDelegationServerProtocol :: HyperTextProtocol
    , cDelegationIpFile         :: FilePath
    } deriving (Show, Generic)

instance FromJSON EncoinsRelayConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

initialiseRelayConfig :: FilePath -> IO ()
initialiseRelayConfig relayConfigFp = do
    EncoinsRelayConfig{..} <- decodeOrErrorFromFile relayConfigFp
    initialiseIpFile cDelegationIpFile

initialiseIpFile :: FilePath -> IO ()
initialiseIpFile ipFileFp = unlessM (doesFileExist ipFileFp) $ do
    putStrLn "relay IP file doesn't exists"
    putStrLn "please enter your relay IP"
    relayIp <- getLine
    addMissingDirectories ipFileFp
    writeFileJSON ipFileFp relayIp >>= either print pure