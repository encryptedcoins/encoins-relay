{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-missing-fields     #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Encoins.Relay.DelegationSpec where

import           Control.Monad                      (replicateM)
import qualified Data.ByteString                    as BS
import           Data.Function                      (on)
import           Data.Functor                       ((<&>))
import           Data.Functor.Identity              (Identity (runIdentity))
import           Data.List                          (sort, sortBy)
import           Data.Maybe                         (fromJust)
import           Data.String                        (IsString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Encoins.Relay.Apps.Delegation.Main (DelegationHandle (..), findDelegators, isValidIp)
import           Encoins.Relay.Server.Delegation    (Delegation (..))
import           Ledger                             (Address (..), Datum (..), DatumHash, PubKeyHash (..), Slot (..), TxId (..),
                                                     TxOutRef (..))
import           Plutus.V1.Ledger.Credential        (Credential (PubKeyCredential), StakingCredential (StakingHash))
import           Plutus.V2.Ledger.Api               (toBuiltin, toBuiltinData)
import           PlutusAppsExtra.Utils.Datum        (hashDatum)
import           PlutusAppsExtra.Utils.Kupo         (KupoDatumType (..), KupoResponse (..), SlotWithHeaderHash (..))
import           Test.Hspec                         (Spec, describe, it, shouldBe)
import           Test.QuickCheck                    (Arbitrary (..), Gen, Property, Testable (property), choose, generate, oneof,
                                                     suchThat)

spec :: Spec
spec = describe "encoins-delegation" $ it "find delegators IP's" propDelegation

propDelegation :: Property
propDelegation = property $ \(args :: [TestArg]) -> do
    handle <- generate $ mkTestDelegationHandle args
    let res  = runIdentity $ findDelegators "" handle 0 0
    sort res `shouldBe` expectedResult args

expectedResult :: [TestArg] -> [Delegation]
expectedResult = sort . map mkDeleg . filter taTxSignatureIsRight
    where
        mkDeleg TestArg{..} = Delegation taCredential taAddressStakeKey (TxOutRef taTxId taTxIdX) taCreatedAt (unDelegIp taDelegIp)

data TestArg = TestArg
    { taCredential         :: Credential
    , taAddressStakeKey    :: PubKeyHash
    , taDelegIp            :: DelegIp
    , taTokenAmount        :: Integer
    , taTxId               :: TxId
    , taTxIdX              :: Integer
    , taTxSignatureIsRight :: Bool
    , taDatum              :: Datum
    , taDatumHash          :: DatumHash
    , taCreatedAt          :: Slot
    } deriving (Show, Eq)

argToKupoResponse :: TestArg -> KupoResponse
argToKupoResponse TestArg{..} =
    let krDatumHash   = Just taDatumHash
        krAddress     = Address taCredential (Just $ StakingHash $ PubKeyCredential taAddressStakeKey)
        krTxId        = taTxId
        krOutputIndex = taTxIdX
        krCreatedAt   = SlotWithHeaderHash taCreatedAt undefined
        krValue       = mempty
        krDatumType   = Nothing
        krScriptHash  = Nothing
        krSpentAt     = Nothing
    in KupoResponse{..}

instance Arbitrary TestArg where
    arbitrary = do
        stakeKeyBs <- BS.pack <$> replicateM 28 arbitrary
        taDelegIp <- arbitrary
        taTokenAmount <- choose (0, 100_000_000_000_000)
        taTxId <- TxId . toBuiltin . BS.pack <$> replicateM 32 arbitrary
        taTxIdX <- choose (1, 40)
        taTxSignatureIsRight <- choose @Int (1, 50) <&> \case
            50 -> False
            _  -> True
        taCredential <- arbitrary
        taCreatedAt <- abs <$> arbitrary
        let taAddressStakeKey = PubKeyHash $ toBuiltin stakeKeyBs
            taDatum = Datum $ toBuiltinData $ map toBuiltin ["ENCOINS", "Delegate", stakeKeyBs, T.encodeUtf8 $ unDelegIp taDelegIp]
            taDatumHash = fromJust $ hashDatum taDatum
        pure TestArg{..}

newtype DelegIp = DelegIp {unDelegIp :: Text}
    deriving newtype (Show, Eq, Ord, IsString)

instance Arbitrary DelegIp where
    arbitrary = fmap DelegIp $ flip suchThat isValidIp $  T.intercalate "." . fmap (T.pack . show) <$> replicateM 4 (choose @Int (0, 255))

instance Arbitrary KupoResponse where
    arbitrary = do
        krTxId        <- arbitrary
        krOutputIndex <- arbitrary
        krAddress     <- arbitrary
        krValue       <- arbitrary
        krDatumHash   <- arbitrary
        krDatumType   <- arbitrary
        krScriptHash  <- arbitrary
        krCreatedAt   <- arbitrary
        krSpentAt     <- arbitrary
        pure $ KupoResponse{..}

instance Arbitrary SlotWithHeaderHash where
    arbitrary = do
        swhhSlot <- arbitrary
        pure $ SlotWithHeaderHash{..}

instance Arbitrary KupoDatumType where
    arbitrary = oneof $ pure <$> [KupoDatumHash, KupoDatumInline]

mkTestDelegationHandle :: [TestArg] -> Gen (DelegationHandle Identity)
mkTestDelegationHandle args = do
    mockResponses <- arbitrary
    balance <- arbitrary
    let delegResponses = argToKupoResponse <$> args
        responses = sortBy (compare `on` (swhhSlot . krCreatedAt)) (mockResponses <> delegResponses)

    let dhGetResponses _ _ = pure responses
        dhGetDatumByHash dh = pure $ case filter ((== dh) . taDatumHash) args of
            [ta] -> Just $ taDatum ta
            _    -> Nothing
        dhGetTokenBalance _ _ (StakingHash (PubKeyCredential pkh)) = pure $ case filter ((== pkh) . taAddressStakeKey) args of
            [ta] -> taTokenAmount ta
            _    -> 0
        dhGetTokenBalance _ _ _ = balance
        dhCheckTxSignature txId _ = pure $ case filter ((== txId) . taTxId) args of
            [ta] -> taTxSignatureIsRight ta
            _    -> False
        dhMkLog = const (pure ())
        dhWithResultSaving = const id
    pure $ DelegationHandle{..}