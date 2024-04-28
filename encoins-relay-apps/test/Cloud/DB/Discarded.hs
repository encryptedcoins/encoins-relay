{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}


module Cloud.DB.Discarded
  ( testDiscarded
  ) where

import           Cloud.DB.Utility                          (testSequential,
                                                            testSession, withDb)
import           Encoins.Relay.Apps.Cloud.PostgreSQL.Query
import           Encoins.Relay.Apps.Cloud.Types

import           Data.ByteString                           (ByteString)
import           Data.String.Here.Uninterpolated           (here)
import qualified Data.Vector                               as V
import qualified Hasql.Pool                                as P
import           Test.Tasty                                (TestTree, testGroup)
import           Test.Tasty.HUnit                          (testCase)

testDiscarded :: IO ()
testDiscarded = withDb initDiscarded discardedTestTree

-- 'after' inside of testSequential is used to make tests sequential
-- TODO: in tasty-1.5 there is sequentialTestGroup to improve tests
-- instead of 'after'
discardedTestTree :: IO P.Pool -> TestTree
discardedTestTree pool = testGroup "Sequential tests for discarded table"
  [ testCase "0. selectAllDiscardedTokens from empty table" $
      testSession pool
        selectAllDiscardedTokensS
        (Right V.empty)
  , testSequential pool
      "0. selectAllDiscardedTokens from empty table"
      "1. insertDiscardedTokensS to empty table"
      (insertDiscardedTokensS $ V.fromList
        [ (MkAssetName "token1", 80)
        , (MkAssetName "token2", 85)
        , (MkAssetName "token3", 90)
        , (MkAssetName "token4", 95)
        , (MkAssetName "token5", 100)
        , (MkAssetName "token5", 105)
        ])
      (Right ())

  , testSequential pool
      "1. insertDiscardedTokensS to empty table"
      "2. selectAllDiscardedTokens from full table"
      selectAllDiscardedTokensS
      (Right $ V.fromList
        [ (MkAssetName "token1")
        , (MkAssetName "token2")
        , (MkAssetName "token3")
        , (MkAssetName "token4")
        , (MkAssetName "token5")
        ])

  , testSequential pool
      "2. selectAllDiscardedTokens from full table"
      "3. selectAllDiscardedTokens before 95 sec"
      (selectStaleDiscardedTokensS $ fromIntegral @Integer 95)
      (Right $ V.fromList
        [ (MkAssetName "token1")
        , (MkAssetName "token2")
        , (MkAssetName "token3")
        ])

  , testSequential pool
      "3. selectAllDiscardedTokens before 95 sec"
      "4. selectAllDiscardedTokens before 120 sec"
      (selectStaleDiscardedTokensS $ fromIntegral @Integer 120)
      (Right $ V.fromList
        [ (MkAssetName "token1")
        , (MkAssetName "token2")
        , (MkAssetName "token3")
        , (MkAssetName "token4")
        , (MkAssetName "token5")
        ])

  , testSequential pool
      "4. selectAllDiscardedTokens before 120 sec"
      "5. selectAllDiscardedTokens before 70 sec"
      (selectStaleDiscardedTokensS $ fromIntegral @Integer 70)
      (Right $ V.empty)

  , testSequential pool
      "5. selectAllDiscardedTokens before 70 sec"
      "6. deleteDiscardedLinksS"
      (deleteDiscardedLinksS $ V.fromList
        [ (MkAssetName "token1")
        , (MkAssetName "token5")
        ])
      (Right ())

  , testSequential pool
      "6. deleteDiscardedLinksS"
      "7. selectAllDiscardedTokens before 95 sec"
      (selectStaleDiscardedTokensS $ fromIntegral @Integer 95)
      (Right $ V.fromList
        [ (MkAssetName "token2")
        , (MkAssetName "token3")
        ])

  , testSequential pool
      "7. selectAllDiscardedTokens before 95 sec"
      "8. selectAllDiscardedTokens before 120 sec"
      (selectStaleDiscardedTokensS $ fromIntegral @Integer 120)
      (Right $ V.fromList
        [ (MkAssetName "token2")
        , (MkAssetName "token3")
        , (MkAssetName "token4")
        ])

  ]

initDiscarded :: ByteString
initDiscarded = [here|
CREATE TABLE IF NOT EXISTS discarded (
  "id" serial not null primary key,
  "asset_name" text not null,
  "discard_time" bigint not null,
  UNIQUE(asset_name)
);
|]
