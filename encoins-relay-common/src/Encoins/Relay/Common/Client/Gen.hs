{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Encoins.Relay.Common.Client.Gen where

import           Cardano.Crypto.DSIGN.Class    (DSIGNAlgorithm (..), seedSizeDSIGN)
import           Cardano.Crypto.DSIGN.Ed25519  (Ed25519DSIGN)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Data                     (Proxy)
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import           Ledger                        (NetworkId)
import           Plutus.PAB.Arbitrary          ()
import           Plutus.V1.Ledger.Api          (CurrencySymbol (..), fromBuiltin)
import           PlutusAppsExtra.Utils.Address (addressToBech32)
import           Test.Crypto.Util              (Message, arbitrarySeedOfSize)
import           Test.QuickCheck               (Arbitrary (..), generate)
import           Text.Hex                      (encodeHex)

randomAddressBech32Text :: MonadIO m => NetworkId -> m Text
randomAddressBech32Text network = liftIO $
    generate arbitrary >>= maybe (randomAddressBech32Text network) pure . addressToBech32 network

randomCSText :: IO Text
randomCSText = do
    CurrencySymbol cs <- generate arbitrary
    pure $ encodeHex $ fromBuiltin cs

randomSignatureText :: IO Text
randomSignatureText = do
    msg :: Message <- generate arbitrary
    sig :: SigDSIGN Ed25519DSIGN <- generate $ signDSIGN () msg
        <$> (genKeyDSIGN <$> arbitrarySeedOfSize (seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)))
    pure $ encodeHex $ rawSerialiseSigDSIGN sig

randomPubKeyText :: IO Text
randomPubKeyText = do
    vk <- generate $ deriveVerKeyDSIGN @Ed25519DSIGN . genKeyDSIGN
        <$> arbitrarySeedOfSize (seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN))
    pure $ encodeHex $ rawSerialiseVerKeyDSIGN vk