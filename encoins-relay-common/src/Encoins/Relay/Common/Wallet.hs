{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Common.Wallet where

import           Cardano.Mnemonic          (MkSomeMnemonic (mkSomeMnemonic), MkSomeMnemonicError, SomeMnemonic (..), mnemonicToText)
import           Control.Applicative       ((<|>))
import           Control.Exception         (Exception, SomeException, handle)
import           Control.Monad             ((>=>))
import           Control.Monad.Catch       (throwM)
import           Crypto.Cipher.AES         (AES256)
import           Crypto.Cipher.Types       (BlockCipher (..), Cipher (cipherInit), IV, makeIV)
import           Crypto.Error              (CryptoError (..), CryptoFailable (..))
import qualified Crypto.Hash               as Hash
import           Crypto.Hash.Algorithms    (MD5)
import qualified Crypto.Random.Types       as CRT
import           Data.Aeson                (FromJSON (..), KeyValue ((.=)), ToJSON (..), (.:))
import qualified Data.Aeson                as J
import           Data.Bifunctor            (Bifunctor (bimap, first))
import           Data.ByteArray            (ByteArray)
import qualified Data.ByteArray            as BA
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.Either.Extra         (maybeToEither)
import           Data.String               (IsString (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import           Encoins.Relay.Common      (decodeOrErrorFromFile)
import           GHC.Generics              (Generic)
import           Ledger                    (Passphrase (..))
import           PlutusAppsExtra.IO.Wallet (RestoredWallet (..))
import qualified Text.Hex                  as T
import           Text.Hex                  (encodeHex)

data ServerWallet
    = UnEncrypted RestoredWallet
    | Encrypted EncryptedWallet
    deriving Show

instance FromJSON ServerWallet where
    parseJSON v = Encrypted   <$> parseJSON v
              <|> UnEncrypted <$> parseJSON v

loadWallet :: FilePath -> IO RestoredWallet
loadWallet = decodeOrErrorFromFile >=> \case
        UnEncrypted w -> pure w
        Encrypted w   -> handleDecrypt w $ decrypt w
    where
        handleDecrypt w = handle $ \(_ :: SomeException) -> do
            putStrLn "Invalid passphrase."
            handleDecrypt w $ decrypt w
        decrypt w = do
            putStrLn "Enter passphrase:"
            T.getLine >>= either throwM pure . decryptWallet w

-- | The passphrase must contain only utf8 characters.
data EncryptedWallet = EncryptedWallet
    { ewName     :: Text
    , ewIVAES256 :: IV AES256
    , ewMnemonic :: ByteString
    } deriving (Generic, Eq)

instance ToJSON EncryptedWallet where
    toJSON EncryptedWallet{..} = J.object
        [ "name"     .= ewName
        , "IV"       .= T.encodeHex (BS.pack $ BA.unpack ewIVAES256)
        , "mnemonic" .= T.encodeHex ewMnemonic
        ]

instance FromJSON EncryptedWallet where
    parseJSON = J.withObject "encrypted wallet" $ \o -> do
        let mbFail f = maybe (fail f) pure
        ewName     <- o .: "name"
        ewIVAES256 <- o .: "IV"
            >>= mbFail "decodeHex IV" . T.decodeHex
            >>= mbFail "make IV" . makeIV
        ewMnemonic <- o .: "mnemonic"
            >>= mbFail "decodeHex mnemonic" . T.decodeHex
        pure EncryptedWallet{..}

instance Show EncryptedWallet where
    show EncryptedWallet{..} = T.unpack ewName <> ":\n" <> show ewMnemonic

data WalletEncryptionError
    = IVInitializingError
    | SomeCryptoError CryptoError
    | PassphraseDecodingError -- TextDecodingError
    | MnemonicDecodingError (MkSomeMnemonicError '[24])
    deriving (Show, Exception, Eq)

data Key c a where
    Key :: (BlockCipher c, ByteArray a) => a -> Key c a

encryptWallet :: RestoredWallet -> IO (Either WalletEncryptionError EncryptedWallet)
encryptWallet RestoredWallet{..} = do
    eitherIV <- maybeToEither IVInitializingError <$> genRandomIV (undefined :: AES256)
    pure $ do
        iV <- eitherIV
        let mnemonicBS = mnemonicToBytes mnemonicSentence
            secretKey = passphraseTextToKey $ encodeHex $ unPassphrase passphrase
        mnemonicE <- bimap SomeCryptoError (\c -> ctrCombine c iV mnemonicBS) $ initCipher secretKey
        pure $ EncryptedWallet name iV mnemonicE

decryptWallet :: EncryptedWallet -> Text -> Either WalletEncryptionError RestoredWallet
decryptWallet EncryptedWallet{..} passphraseText = do
    let secretKey = passphraseTextToKey passphraseText
        passphrase = fromString $ T.unpack passphraseText
    mnemonicBS <- bimap SomeCryptoError (\c -> ctrCombine c ewIVAES256 ewMnemonic) $ initCipher secretKey
    mnemonic   <- first MnemonicDecodingError $ bytesToMnemonic mnemonicBS
    pure $ RestoredWallet ewName mnemonic passphrase

passphraseTextToKey :: Text -> Key AES256 ByteString
passphraseTextToKey = Key . fromString . show . Hash.hash @ByteString @MD5 . fromString . T.unpack

mnemonicToBytes :: SomeMnemonic -> ByteString
mnemonicToBytes (SomeMnemonic m) = T.encodeUtf8 $ T.unwords $ mnemonicToText m

bytesToMnemonic :: ByteString -> Either (MkSomeMnemonicError '[24]) SomeMnemonic
bytesToMnemonic = mkSomeMnemonic @'[24] . T.words . T.decodeUtf8

genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
    bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: c)
    return $ makeIV bytes

initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
    CryptoFailed e -> Left e
    CryptoPassed a -> Right a
