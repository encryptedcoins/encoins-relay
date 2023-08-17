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

import           Cardano.Server.Client.Handle (HasServantClientEnv)
import           Cardano.Server.Config        (ServerEndpoint (ServerTxE))
import           Control.Lens                 (Bifunctor (bimap), view)
import           Control.Monad                (join, replicateM)
import           Control.Monad.State          (State, StateT, evalState, evalStateT, runState)
import qualified Data.ByteString              as BS
import           Data.Function                (on)
import           Data.Functor                 ((<&>))
import           Data.Functor.Identity        (Identity (runIdentity))
import           Data.List                    (sort, sortBy)
import           Data.Maybe                   (fromJust)
import           Data.String                  (IsString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Encoins.Relay.Client.Client  (TxClientCosntraints, txClientDelegation)
import           Encoins.Relay.Delegation     (DelegationConfig (DelegationConfig, dcMinTokenAmount), DelegationHandle (..),
                                               findDelegators, isValidIp)
import           Internal                     (runEncoinsServerM)
import           Ledger                       (Address (..), Datum (Datum, getDatum), DatumHash, PubKeyHash (..), Slot (..),
                                               TxId (..))
import           Plutus.PAB.Arbitrary         ()
import           Plutus.V1.Ledger.Credential  (Credential (PubKeyCredential), StakingCredential (StakingHash))
import           Plutus.V2.Ledger.Api         (Data (..), builtinDataToData, dataToBuiltinData, toBuiltin, toBuiltinData)
import           PlutusAppsExtra.Utils.Datum  (hashDatum)
import           PlutusAppsExtra.Utils.Kupo   (KupoDatumType (..), KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusTx.Builtins            (encodeUtf8)
import           System.Random                (randomRIO)
import           Test.Hspec                   (Expectation, Spec, context, describe, it, shouldBe)
import           Test.QuickCheck              (Arbitrary (..), Gen, Property, Testable (property), choose, generate, oneof,
                                               shuffle, suchThat, withMaxSuccess)

spec :: HasServantClientEnv => Spec
spec = describe "encoins-delegation" $ it "find delegators ips" propDelegation

propDelegation :: Property
propDelegation = property $ \(args :: [TestArg]) -> do
    let balances = map taTokenAmount args
    minTokenAmount <- generate (choose (minimum balances, maximum balances))
    handle <- generate $ mkTestDelegationHandle args
    let conf = DelegationConfig {dcMinTokenAmount = minTokenAmount}
        res  = runIdentity $ findDelegators conf handle
    sort res `shouldBe` expectedResult minTokenAmount args

expectedResult :: Integer -> [TestArg] -> [Text]
expectedResult minTokenAmount = sort . map (unDelegIp . taDelegIp) . filter f
    where
        f TestArg{..} = taTokenAmount >= minTokenAmount && taTxSignatureIsRight

data TestArg = TestArg
    { taAddress            :: Address
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
        krAddress     = taAddress
        krTxId        = taTxId
        krOutputIndex = taTxIdX
        krCreatedAt   = SlotWithHeaderHash taCreatedAt undefined
    in KupoResponse{..}

instance Arbitrary TestArg where
    arbitrary = do
        taAddressStakeKey <- PubKeyHash . toBuiltin . BS.pack <$> replicateM 28 arbitrary
        taDelegIp <- arbitrary
        taTokenAmount <- choose (0, 100_000_000_000_000)
        taTxId <- TxId . toBuiltin . BS.pack <$> replicateM 32 arbitrary
        taTxIdX <- choose (1, 40)
        taTxSignatureIsRight <- choose @Int (1, 50) <&> \case
            50 -> False
            _  -> True
        addrCred <- arbitrary
        taCreatedAt <- abs <$> arbitrary
        let taAddress = Address addrCred (Just $ StakingHash (PubKeyCredential taAddressStakeKey))
            taDatum = Datum $ toBuiltinData $ join bimap (encodeUtf8 . toBuiltin) ("ENCS Delegation", unDelegIp taDelegIp)
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

    let getResponses = pure responses
        getDatumByHash dh = pure $ case filter ((== dh) . taDatumHash) args of
            [ta] -> Just $ taDatum ta
            _    -> Nothing
        getTokenBalance _ _ (StakingHash (PubKeyCredential pkh)) = pure $ case filter ((== pkh) . taAddressStakeKey) args of
            [ta] -> taTokenAmount ta
            _    -> 0
        getTokenBalance _ _ _ = balance
        checkTxSignature txId _ = pure $ case filter ((== txId) . taTxId) args of
            [ta] -> taTxSignatureIsRight ta
            _    -> False
        mkLog = const (pure ())
        withResultSaving = const id
    pure $ DelegationHandle{..}