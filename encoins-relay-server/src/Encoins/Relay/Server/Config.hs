{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Encoins.Relay.Server.Config where

import           Cardano.Api                        (NetworkId, prettyPrintJSON, writeFileJSON)
import           Cardano.Mnemonic                   (MkSomeMnemonic (..))
import           Control.Applicative                (Applicative (..))
import           Control.Exception                  (SomeException)
import           Control.Lens                       ((&), (.~))
import           Control.Monad                      (join, when, (<=<), (>=>))
import           Control.Monad.Catch                (try)
import           Control.Monad.Extra                (unlessM, whenM)
import           Data.Aeson                         (FromJSON (..), ToJSON (..), genericParseJSON, (.=))
import qualified Data.Aeson                         as J
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Aeson.Lens                    (key)
import qualified Data.ByteString                    as BS
import           Data.Either.Extra                  (eitherToMaybe)
import           Data.List.Extra                    (breakOnEnd, notNull)
import           Data.List.NonEmpty                 (NonEmpty ((:|)), nonEmpty)
import           Data.Maybe                         (fromJust, fromMaybe, isJust, isNothing)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Encoins.Relay.Common               (decodeOrErrorFromFile)
import           Encoins.Relay.Common.Server        (HyperTextProtocol)
import           GHC.Generics                       (Generic)
import           Katip                              (Environment, Severity, Verbosity)
import           Plutus.V2.Ledger.Api               (Address, CurrencySymbol, TokenName, TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex      (ChainIndexProvider)
import qualified PlutusAppsExtra.IO.ChainIndex      as CI
import           PlutusAppsExtra.IO.Tx              (TxProvider)
import qualified PlutusAppsExtra.IO.Tx              as Tx
import           PlutusAppsExtra.IO.Wallet          (RestoredWallet (..), WalletProvider, restoreWalletFromFile)
import qualified PlutusAppsExtra.IO.Wallet          as Wallet
import           PlutusAppsExtra.IO.Wallet.Internal (addressFromMnemonic)
import           PlutusAppsExtra.Utils.Address      (addressToBech32, bech32ToAddress)
import           PlutusTx.Builtins                  (BuiltinByteString)
import           System.Directory                   (createDirectoryIfMissing, doesFileExist)

-- loadEncoinsRelayConfig :: MonadIO m => EncoinsRelayConfig -> m EncoinsRelayConfig
-- loadEncoinsRelayConfig c = liftIO $ decodeOrErrorFromFile $ cAuxiliaryEnvFile c

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr1q8u2rh5uud6yzmhq0de7vt7p0rvqpfadwnee3tjnz2tl4rct6qt03wjc2lfwyqnd54gwfdey50s7342e3jl6kxwww4kqzfah2x"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

data EncoinsRelayConfig = MkEncoinsRelayConfig
    -- Server
    { cHost                     :: Text
    , cPort                     :: Int
    , cHyperTextProtocol        :: HyperTextProtocol
    , cDiagnosticsInterval      :: Maybe Int
    -- TODO:
    -- , cActiveEndpoints          :: [ServerEndpoint]

    -- Encoins server
    , cRefStakeOwner            :: TxOutRef
    , cRefBeacon                :: TxOutRef
    , cValidatorStakeKey        :: BuiltinByteString

    -- Clean utxos check
    , cMinUtxosNumber           :: Int
    , cMaxUtxosNumber           :: Int

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

    -- Logger
    , cEnvironment              :: Environment
    , cVerbosity                :: Verbosity
    , cSeverity                 :: Severity
    , cFormatMessage            :: Bool

    -- Providers
    , cProtocolParametersFile   :: FilePath
    , cSlotConfigFile           :: FilePath
    , cWalletFile               :: FilePath
    , cNetworkId                :: NetworkId
    , cCollateral               :: Maybe TxOutRef
    , cDataProviders            :: ConfigDataProviders

    -- Maestro
    , cMaestroTokenFilePath     :: Maybe FilePath

    -- Blockfrost
    , cBfTokenFilePath          :: Maybe FilePath

    } deriving (Show, Generic)

instance FromJSON EncoinsRelayConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ConfigDataProviders
    = Lightweight FilePath
    -- ^ File with addresses tracked
    | Cardano FilePath
    -- ^ Node file path
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

dataProvidersFromConfigDataProviders :: ConfigDataProviders -> IO (WalletProvider, ChainIndexProvider, TxProvider)
dataProvidersFromConfigDataProviders = \case
    Lightweight fp -> do
        addrs <- fromMaybe (error "Unparsable addresses for lightweight wallet provider.") <$> readAddrs fp
        pure (Wallet.Lightweight addrs, CI.Maestro, Tx.Maestro)
    Cardano fp -> pure (Wallet.Cardano, CI.Kupo, Tx.Cardano fp)
  where
    readAddrs = fmap (>>= (mapM bech32ToAddress >=> nonEmpty)) . J.decodeFileStrict @[Text]

------------------------------------------------------------------- Inintialisation -------------------------------------------------------------------

-- | Ask user to type all missing information
initializeConfig :: FilePath -> IO ()
initializeConfig configFp = do
    MkEncoinsRelayConfig{..} <- decodeOrErrorFromFile configFp
    sequence_ $ initializeToken "blockfrost" <$> cBfTokenFilePath
    sequence_ $ initializeToken "maestro" <$> cMaestroTokenFilePath
    initializeNode configFp MkEncoinsRelayConfig{..}
    initializeWallet cWalletFile
    initializeWalletProvider MkEncoinsRelayConfig{..}
    initializeIpFile cDelegationIpFile

initializeIpFile :: FilePath -> IO ()
initializeIpFile ipFileFp = unlessM (doesFileExist ipFileFp) $ do
    putStrLn "relay IP file doesn't exists"
    putStrLn "please enter your relay IP"
    relayIp <- getLine
    addMissingDirectories ipFileFp
    writeFileJSON ipFileFp relayIp >>= either print pure

initializeNode :: FilePath -> EncoinsRelayConfig -> IO ()
initializeNode _        MkEncoinsRelayConfig{cDataProviders = Lightweight _} = pure ()
initializeNode configFp MkEncoinsRelayConfig{cDataProviders = Cardano nodeFp} = unlessM (doesFileExist nodeFp) $ do
    putStrLn "you have node-based providers but cardano node socket doesn't exists in specified path"
    putStrLn "enter correct cardano-node socket file path"
    fp <- getLine
    config_val <- decodeOrErrorFromFile @J.Value configFp
    writeJSONPretty configFp $ config_val & key "cDataProviders" .~ toJSON (Cardano fp)
    decodeOrErrorFromFile configFp >>= initializeNode configFp

initializeWallet :: FilePath -> IO ()
initializeWallet cWalletFile =
    unlessM (liftA2 (&&) (doesFileExist cWalletFile) (isJust <$> readWalletFile)) $ do
        putStrLn "wallet doesn't exists or wallet file is corrupted"
        putStrLn "enter your wallet name"
        name <- T.getLine
        mnemonic_txt <- enterMnemonic
        putStrLn "enter your wallet passphrase"
        passphrase <- getLine
        addMissingDirectories cWalletFile
        res <- writeFileJSON cWalletFile $ J.object
            [ "name" .= name
            , "mnemonic_sentence" .= T.words mnemonic_txt
            , "passphrase" .= passphrase
            ]
        either print pure res
  where
    readWalletFile = fmap (join . eitherToMaybe) $ try @_ @SomeException $ J.decodeFileStrict @RestoredWallet cWalletFile
    enterMnemonic = do
        putStrLn "enter your wallet mnemonic (24 words)"
        mnemonic_txt <- T.getLine
        case mkSomeMnemonic @'[ 24 ] $ T.words mnemonic_txt of
            Right _  -> pure mnemonic_txt
            Left err -> print err >> enterMnemonic

initializeWalletProvider :: EncoinsRelayConfig -> IO ()
initializeWalletProvider MkEncoinsRelayConfig{cDataProviders = Cardano _} = pure ()
initializeWalletProvider MkEncoinsRelayConfig{cDataProviders = Lightweight fp, ..} =
    whenM (isNothing <$> readAddrs) $ do
        putStrLn "no addresses specified for lightweight wallet provider or addresses file is corrupted"
        putStrLn "now they will be selected automatically, but you can manually change them at any time"
        putStrLn "keep in mind that each additional address also incurs additional costs in the form of provider credits"
        mnemonic <- mnemonicSentence <$> restoreWalletFromFile cWalletFile
        addrs <- (:| []) <$> addressFromMnemonic cNetworkId mnemonic
        addMissingDirectories fp
        writeFileJSON fp (addressToBech32  cNetworkId <$> addrs) >>= either print pure
  where
    readAddrs =  fmap ((mapM bech32ToAddress >=> nonEmpty) <=< (join . eitherToMaybe))
        $ try @_ @SomeException $ J.decodeFileStrict @[Text] fp

initializeToken :: String -> FilePath -> IO String
initializeToken tokenName fp = do
    unlessM (doesFileExist fp) $ do
        putStrLn $ tokenName <> " token file doesn't exists."
        putStrLn $ "enter your " <> tokenName <> " token."
        token <- getLine
        writeFile fp $ withQuotes token
    readFile fp
  where
    withQuotes x@('"' : _) = x
    withQuotes x           = '"' : x <> "\""

writeJSONPretty :: FilePath -> J.Value -> IO ()
writeJSONPretty fp val = BS.writeFile fp $ prettyPrintJSON val

addMissingDirectories :: FilePath -> IO ()
addMissingDirectories fp = let (dir,_) = breakOnEnd "/" fp in when (notNull dir) $ createDirectoryIfMissing True dir
