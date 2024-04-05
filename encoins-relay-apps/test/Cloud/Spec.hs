{-# LANGUAGE OverloadedStrings #-}

module Cloud.Spec
  (
    getNotDiscardedSpec
  , isDiscardedSpec
  , isDiscardedInListSpec
  ) where

import           Encoins.Relay.Apps.Cloud.Server (getNotDiscarded, isDiscarded,
                                                  isDiscardedInList)
import           Encoins.Relay.Apps.Cloud.Types
import           Ledger                          (Slot (..))
import           PlutusAppsExtra.Utils.Maestro   (AssetMintsAndBurnsData (..),
                                                  AssetMintsAndBurnsResponse (..))

import           Data.Time.Clock.POSIX           (POSIXTime,
                                                  posixSecondsToUTCTime)
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           Test.Hspec                      (Spec, describe, hspec, it,
                                                  shouldBe)


getNotDiscardedSpec :: Spec
getNotDiscardedSpec =
  describe "getNotDiscardedSpec" $ do
    it "There is an intersection" $
      getNotDiscarded savedTokens1 alreadyDiscarded1 `shouldBe` newDiscarded1
    it "Discarded list is empty" $
      getNotDiscarded savedTokens2 alreadyDiscarded2 `shouldBe` newDiscarded2
    it "Discarded list is equal to saved list" $
      getNotDiscarded savedTokens3 alreadyDiscarded3 `shouldBe` newDiscarded3
    it "Saved list is empty" $
      getNotDiscarded savedTokens4 alreadyDiscarded4 `shouldBe` newDiscarded4
    it "All lists are empty" $
      getNotDiscarded savedTokens5 alreadyDiscarded5 `shouldBe` newDiscarded5


savedTokens1 :: Vector (Int,Char)
savedTokens1 = V.fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]

alreadyDiscarded1 :: Vector Int
alreadyDiscarded1 = V.fromList [1,2,3]

newDiscarded1 :: Vector (Int,Char)
newDiscarded1 = V.fromList [(4,'d'),(5,'e'),(6,'f'),(7,'g')]

savedTokens2 :: Vector (Int,Char)
savedTokens2 = V.fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]

alreadyDiscarded2 :: Vector Int
alreadyDiscarded2 = V.fromList []

newDiscarded2 :: Vector (Int,Char)
newDiscarded2 = V.fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]

savedTokens3 :: Vector (Int,Char)
savedTokens3 = V.fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]

alreadyDiscarded3 :: Vector Int
alreadyDiscarded3 = V.fromList [1,2,3,4,5,6,7]

newDiscarded3 :: Vector (Int,Char)
newDiscarded3 = V.fromList []

savedTokens4 :: Vector (Int,Char)
savedTokens4 = V.fromList []

alreadyDiscarded4 :: Vector Int
alreadyDiscarded4 = V.fromList [1,2,3,4,5,6]

newDiscarded4 :: Vector (Int,Char)
newDiscarded4 = V.fromList []

savedTokens5 :: Vector (Int,Char)
savedTokens5 = V.fromList []

alreadyDiscarded5 :: Vector Int
alreadyDiscarded5 = V.fromList [1,2,3,4,5,6]

newDiscarded5 :: Vector (Int,Char)
newDiscarded5 = V.fromList []


isDiscardedSpec :: Spec
isDiscardedSpec =
  describe "isDiscarded" $ do
    it "True when ambrData is empty, saveTime + lag < now" $
      isDiscarded now lag asset1 (dataRes []) `shouldBe` True
    it "False when ambrData is empty, saveTime + lag == now" $
      isDiscarded now lag asset2 (dataRes []) `shouldBe` False
    it "False when ambrData is empty, saveTime + lag > now" $
      isDiscarded now lag asset3 (dataRes []) `shouldBe` False
    it "False when minted state, stateTime + lag < now" $
      isDiscarded now lag asset1 (dataRes [assetData 1 80 33]) `shouldBe` False
    it "False when minted state, stateTime + lag == now" $
      isDiscarded now lag asset1 (dataRes [assetData 1 90 33]) `shouldBe` False
    it "False when minted state, stateTime + lag > now" $
      isDiscarded now lag asset1 (dataRes [assetData 1 100 33]) `shouldBe` False
    it "False when invalid amount, stateTime + lag < now" $
      isDiscarded now lag asset1 (dataRes [assetData 0 80 33]) `shouldBe` False
    it "False when invalid amount, stateTime + lag == now" $
      isDiscarded now lag asset1 (dataRes [assetData 0 90 33]) `shouldBe` False
    it "False when invalid amount, stateTime + lag > now" $
      isDiscarded now lag asset1 (dataRes [assetData 0 100 33]) `shouldBe` False
    it "True when burned state, stateTime + lag < now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 80 33]) `shouldBe` True
    it "False when burned state, stateTime + lag == now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 90 33]) `shouldBe` False
    it "False when burned state, stateTime + lag > now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 100 33]) `shouldBe` False
    it "True when valid Minted and Burned states, stateTime + lag < now" $
      isDiscarded now lag asset1 (dataRes [assetData 1 70 33, assetData (-1) 80 44]) `shouldBe` True
    it "False when valid Minted and Burned states, stateTime + lag == now" $
      isDiscarded now lag asset1 (dataRes [assetData 1 70 33, assetData (-1) 90 44]) `shouldBe` False
    it "False when valid Minted and Burned states, stateTime + lag > now" $
      isDiscarded now lag asset1 (dataRes [assetData 1 70 33, assetData (-1) 100 44]) `shouldBe` False

    it "True when disordered valid Minted and Burned states, stateTime + lag < now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 80 44, assetData 1 70 33]) `shouldBe` True
    it "False when disordered valid Minted and Burned states, stateTime + lag == now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 90 44, assetData 1 70 33]) `shouldBe` False
    it "False when disordered valid Minted and Burned states, stateTime + lag > now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 100 44, assetData 1 70 33]) `shouldBe` False

    it "False when disordered valid Minted and Burned states has invalid slots, stateTime + lag < now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 80 33, assetData 1 70 44]) `shouldBe` False
    it "False when disordered valid Minted and Burned states has invalid slots, stateTime + lag == now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 90 33, assetData 1 70 44]) `shouldBe` False
    it "False when disordered valid Minted and Burned states has invalid slots, stateTime + lag > now" $
      isDiscarded now lag asset1 (dataRes [assetData (-1) 100 33, assetData 1 70 44]) `shouldBe` False

dataRes :: [AssetMintsAndBurnsData] -> AssetMintsAndBurnsResponse
dataRes ds = AssetMintsAndBurnsResponse
    { ambrData = ds
    , ambrCursor = Nothing
    }

assetData :: Integer -> POSIXTime -> Slot -> AssetMintsAndBurnsData
assetData amount time slot = AssetMintsAndBurnsData
    { ambrAmount = amount
    , ambrSlot = slot
    , ambrTimestamp = posixSecondsToUTCTime time
    , ambrTxHash = "tx_hash"
    }

assetName :: AssetName
assetName = MkAssetName "name1"

now :: POSIXTime
now = 100

lag :: POSIXTime
lag = 10

asset1 :: (AssetName, POSIXTime)
asset1 = (assetName, 80)

asset2 :: (AssetName, POSIXTime)
asset2 = (assetName, 90)

asset3 :: (AssetName, POSIXTime)
asset3 = (assetName, 100)


isDiscardedInListSpec :: Spec
isDiscardedInListSpec =
  describe "isDiscardedInList" $ do
    it "Response of maestro is empty, saveTime + lag < now" $
      isDiscardedInList now lag asset1 [] `shouldBe` Just assetName
    it "Response of maestro is empty, saveTime + lag == now" $
      isDiscardedInList now lag asset2 [] `shouldBe` Nothing
    it "Response of maestro is empty, saveTime + lag > now" $
      isDiscardedInList now lag asset3 [] `shouldBe` Nothing
    it "Response of maestro has one valid token" $
      isDiscardedInList now lag asset3 [dataRes [assetData (-1) 80 44, assetData 1 70 33]] `shouldBe` Just assetName
    it "Response of maestro has one invalid token" $
      isDiscardedInList now lag asset3 [dataRes [assetData (-1) 80 33, assetData 1 70 44]] `shouldBe` Nothing
    it "Response of maestro has all valid tokens" $
      isDiscardedInList now lag asset3
        [ dataRes [assetData (-1) 80 44, assetData 1 70 33]
        , dataRes [assetData (-1) 80 33]
        ] `shouldBe` Just assetName
    it "Response of maestro has one valid and one invalid state of tokens" $
      isDiscardedInList now lag asset3
        [ dataRes [assetData (-1) 80 33]
        , dataRes [assetData (-1) 80 33, assetData 1 70 44]
        ] `shouldBe` Nothing
