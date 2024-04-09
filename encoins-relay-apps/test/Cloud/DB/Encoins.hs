{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}


module Cloud.DB.Encoins
  ( testEncoins
  ) where

import           Cloud.DB.Utility                          (testSequential,
                                                            testSession, withDb)
import           Encoins.Relay.Apps.Cloud.PostgreSQL.Query
import           Encoins.Relay.Apps.Cloud.Types

import           Control.Monad                             (void)
import           Data.ByteString                           (ByteString)
import           Data.String.Here.Uninterpolated           (here)
import           Data.Text                                 (Text)
import           Data.Time.Clock.POSIX                     (POSIXTime)
import           Data.Tuple.Extra                          (second)
import           Data.Vector                               (Vector)
import qualified Data.Vector                               as V
import qualified Hasql.Pool                                as P
import           Hasql.Session                             (Session)
import qualified Hasql.Transaction.Sessions                as TS
import           Test.Tasty                                (TestTree, testGroup)
import           Test.Tasty.HUnit                          (testCase)

testEncoins :: IO ()
testEncoins = withDb initEncoins encoinsTestTree

-- 'after' is used to make tests sequential
-- TODO: in tasty-1.5 there is sequentialTestGroup to improve tests
-- instead of 'after'
encoinsTestTree :: IO P.Pool -> TestTree
encoinsTestTree pool = testGroup "Sequential tests for encoins table"
  [ testCase "0. getTokensByName from empty table" $ do
      testSession pool
        (getTokensByNameS (MkAssetName "token1"))
        (Right V.empty)

  , testSequential pool
      "0. getTokensByName from empty table"
      "1. insertOnAbsentS when item does not exist"
       (insertOnAbsentS
         (MkAssetName "token name 1")
         (MkEncryptedSecret "secret1")
         123)
       (Right 1)

  , testSequential pool
      "1. insertOnAbsentS when item does not exist"
      "2. getTokensByName when item exist"
      (getTokensByNameS (MkAssetName "token name 1"))
      (Right $ V.singleton ("token name 1", "secret1"))

  , testSequential pool
      "2. getTokensByName when item exist"
      "3. insert and select all items"
      (eInsertAndSelectAll $ V.fromList
        [ (MkAssetName "token1", MkEncryptedSecret "secret1", 123)
        , (MkAssetName "token2", MkEncryptedSecret "secret2", 123)
        ])
      (Right $ V.fromList
        [ ("token name 1", "secret1")
        , ("token1", "secret1")
        , ("token2", "secret2")
        ])

  , testSequential pool
      "3. insert and select all items"
      "4. getIdOfNameSecretS when item exists"
      (getIdOfNameSecretS
        (MkAssetName "token2")
        (MkEncryptedSecret "secret2"))
      (Right $ Just 3)

  , testSequential pool
      "4. getIdOfNameSecretS when item exists"
      "5. getIdOfNameSecretS when item does not exist"
      (getIdOfNameSecretS
        (MkAssetName "token4")
        (MkEncryptedSecret "secret2"))
      (Right Nothing)

  , testSequential pool
      "5. getIdOfNameSecretS when item does not exist"
      "6. insertOnAbsentS when item exists"
      (insertOnAbsentS
        (MkAssetName "token name 1")
        (MkEncryptedSecret "secret1")
        123)
      (Right 1)

  , testSequential pool
      "6. insertOnAbsentS when item exists"
      "7. insertOnAbsentS when another item exists"
      (insertOnAbsentS
        (MkAssetName "token2")
        (MkEncryptedSecret "secret2")
        123)
      (Right 3)

  , testSequential pool
      "7. insertOnAbsentS when another item exists"
      "8. insertOnAbsentS when token exists and secret does not"
      (insertOnAbsentS
        (MkAssetName "token2")
        (MkEncryptedSecret "secret3")
        123)
      (Right 4)

  , testSequential pool
      "8. insertOnAbsentS when token exists and secret does not"
      "9. getTokensByName when 2 items exist"
      (getTokensByNameS (MkAssetName "token2"))
      (Right $ V.fromList
        [ ("token2", "secret2")
        , ("token2", "secret3")
        ])

  , testSequential pool
      "9. getTokensByName when 2 items exist"
      "10. count rows"
      countEncoinsRowsS
      (Right 4)

  , testSequential pool
      "10. count rows"
      "11. getAllSavedTokensS"
      getAllSavedTokensS
      (Right $ V.fromList
        [ ("token name 1", "secret1")
        , ("token1", "secret1")
        , ("token2", "secret2")
        , ("token2", "secret3")
        ])

  , testSequential pool
      "11. getAllSavedTokensS"
      "12. selectUniqSavedTokensS"
      selectUniqSavedTokensS
      (Right $ V.fromList $ map (second (fromIntegral @Integer))
        [ (MkAssetName "token name 1", 123)
        , (MkAssetName "token1", 123)
        , (MkAssetName "token2", 123)
        ])

  , testSequential pool
      "12. selectUniqSavedTokensS"
      "13. deleteTokensByNameS unique item"
      (deleteTokensByNameS $ MkAssetName "token1")
      (Right $ V.fromList
        [ ("token1", "secret1")
        ])

  , testSequential pool
      "13. deleteTokensByNameS unique item"
      "14. getAllSavedTokensS after delete"
      getAllSavedTokensS
      (Right $ V.fromList
        [ ("token name 1", "secret1")
        , ("token2", "secret2")
        , ("token2", "secret3")
        ])

  , testSequential pool
      "14. getAllSavedTokensS after delete"
      "15. deleteTokensByNameS not unique items"
      (deleteTokensByNameS $ MkAssetName "token2")
      (Right $ V.fromList
        [ ("token2", "secret2")
        , ("token2", "secret3")
        ])

  , testSequential pool
      "15. deleteTokensByNameS not unique items"
      "16. getAllSavedTokensS after second delete"
      getAllSavedTokensS
      (Right $ V.fromList
        [ ("token name 1", "secret1")
        ])

  , testSequential pool
      "16. getAllSavedTokensS after second delete"
      "17. insert  7items and select all items"
      (eInsertMany $ V.fromList
        [ (MkAssetName "token1", MkEncryptedSecret "secret1", 123)
        , (MkAssetName "token2", MkEncryptedSecret "secret2", 123)
        , (MkAssetName "token3", MkEncryptedSecret "secret31", 123)
        , (MkAssetName "token3", MkEncryptedSecret "secret32", 123)
        , (MkAssetName "token3", MkEncryptedSecret "secret33", 123)
        , (MkAssetName "token4", MkEncryptedSecret "secret4", 123)
        , (MkAssetName "token5", MkEncryptedSecret "secret5", 123)
        ])
      (Right ())

  , testSequential pool
      "17. insert  7items and select all items"
      "18. getAllSavedTokensS after insert"
      getAllSavedTokensS
      (Right $ V.fromList
        [ ("token name 1", "secret1")
        , ("token1", "secret1")
        , ("token2", "secret2")
        , ("token3", "secret31")
        , ("token3", "secret32")
        , ("token3", "secret33")
        , ("token4", "secret4")
        , ("token5", "secret5")
        ])

  , testSequential pool
      "18. getAllSavedTokensS after insert"
      "19. deleteDiscardedTokens"
      (deleteDiscardedTokensS $ V.fromList
        [ MkAssetName "token2"
        , MkAssetName "token3"
        , MkAssetName "token4"
        ])
      (Right ())

  , testSequential pool
      "19. deleteDiscardedTokens"
      "20. getAllSavedTokensS after delete discarded tokens"
      getAllSavedTokensS
      (Right $ V.fromList
        [ ("token name 1", "secret1")
        , ("token1", "secret1")
        , ("token5", "secret5")
        ])
  ]

eInsertAndSelectAll :: Vector (AssetName, EncryptedSecret, POSIXTime)
  -> Session (Vector (Text, Text))
eInsertAndSelectAll rows =
  TS.transaction TS.Serializable TS.Write $ do
    V.forM_ rows $ \(name', secret, time') ->
      void $ insertTokenT name' secret time'
    getAllSavedTokensT

eInsertMany :: Vector (AssetName, EncryptedSecret, POSIXTime)
  -> Session ()
eInsertMany rows =
  TS.transaction TS.Serializable TS.Write $ do
    V.forM_ rows $ \(name', secret, time') ->
      void $ insertTokenT name' secret time'

initEncoins :: ByteString
initEncoins = [here|
CREATE TABLE IF NOT EXISTS encoins (
  "id" serial not null primary key,
  "asset_name" text not null,
  "encrypted_secret" text not null,
  "save_time" bigint not null,
  UNIQUE(asset_name, encrypted_secret)
);
|]
