{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Apps.Cloud.PostgreSQL.Query where

import           Encoins.Relay.Apps.Cloud.PostgreSQL.Statement
import           Encoins.Relay.Apps.Cloud.Types

import           Data.Int
import           Data.Text                                     (Text)
import           Data.Time.Clock.POSIX                         (POSIXTime)
import           Data.Vector                                   (Vector)
import qualified Hasql.CursorQuery.Sessions                    as CQS
import           Hasql.Session                                 (Session)
import           Hasql.Transaction                             (Transaction)
import qualified Hasql.Transaction                             as T
import qualified Hasql.Transaction.Sessions                    as TS


-- * Sessions

-- ** Select

-- *** From encoins

getTokensByNameS :: AssetName -> Session (Vector (Text, Text))
getTokensByNameS assetName = TS.transaction TS.ReadCommitted TS.Read $
  getTokensByNameT assetName

getIdOfNameSecretS :: AssetName
  -> EncryptedSecret
  -> Session (Maybe Int32)
getIdOfNameSecretS assetName encryptedSecret = TS.transaction TS.ReadCommitted TS.Read $
  getIdOfNameSecretT assetName encryptedSecret

getAllSavedTokensS :: Session (Vector (Text, Text))
getAllSavedTokensS = TS.transaction TS.ReadCommitted TS.Read $
  getAllSavedTokensT

insertOnAbsentS :: AssetName -> EncryptedSecret -> POSIXTime -> Session Int32
insertOnAbsentS assetName encryptedSecret createTime =
  TS.transaction TS.Serializable TS.Write $ do
    mId <- getIdOfNameSecretT assetName encryptedSecret
    case mId of
      Nothing -> insertTokenT assetName encryptedSecret createTime
      Just i  -> pure i

countEncoinsRowsS :: Session Int
countEncoinsRowsS = CQS.cursorQuery () countEncoinsRows

selectUniqSavedTokensS :: Session (Vector (AssetName, POSIXTime))
selectUniqSavedTokensS = TS.transaction TS.ReadCommitted TS.Read
  selectUniqSavedTokensT

-- *** From discarded

selectStaleDiscardedTokensS :: POSIXTime -> Session (Vector AssetName)
selectStaleDiscardedTokensS = TS.transaction TS.ReadCommitted TS.Read .
  selectStaleDiscardedTokensT

selectAllDiscardedTokensS :: Session (Vector AssetName)
selectAllDiscardedTokensS = TS.transaction TS.ReadCommitted TS.Read
  selectAllDiscardedTokensT

-- ** Insert

-- *** Into encoins

insertTokenS :: AssetName
  -> EncryptedSecret
  -> POSIXTime
  -> Session Int32
insertTokenS assetName encryptedSecret createTime = TS.transaction TS.Serializable TS.Write $
  insertTokenT assetName encryptedSecret createTime

-- *** Into discarded

insertDiscardedTokensS :: Vector (AssetName, POSIXTime) -> Session ()
insertDiscardedTokensS = TS.transaction TS.Serializable TS.Write .
  insertDiscardedTokensT

-- ** Delete

-- *** From encoins

deleteTokensByNameS :: AssetName -> Session (Vector (Text, Text))
deleteTokensByNameS assetName = TS.transaction TS.ReadCommitted TS.Write $
  deleteTokensByNameT assetName

deleteDiscardedTokensS :: Vector AssetName -> Session ()
deleteDiscardedTokensS vDiscardedTokens = TS.transaction TS.ReadCommitted TS.Write $
  deleteDiscardedTokensT vDiscardedTokens

-- *** From discarded

deleteDiscardedLinksS :: Vector AssetName -> Session ()
deleteDiscardedLinksS vDiscardedTokens = TS.transaction TS.ReadCommitted TS.Write $
  deleteDiscardedLinksT vDiscardedTokens

-- *** From encoins and discarded

deleteDiscardedTokensAndLinksS :: Vector AssetName -> Session ()
deleteDiscardedTokensAndLinksS vDiscardedTokens = TS.transaction TS.Serializable TS.Write $ do
  deleteDiscardedTokensT vDiscardedTokens
  deleteDiscardedLinksT vDiscardedTokens

-- * Transaction

-- ** Select

-- *** From encoins

getTokensByNameT :: AssetName -> Transaction (Vector (Text, Text))
getTokensByNameT assetName = T.statement assetName getTokensByName

getIdOfNameSecretT :: AssetName
  -> EncryptedSecret
  -> Transaction (Maybe Int32)
getIdOfNameSecretT assetName encryptedSecret =
  T.statement (assetName, encryptedSecret) getIdOfNameSecret

getAllSavedTokensT :: Transaction (Vector (Text, Text))
getAllSavedTokensT = T.statement () getAllSavedTokens

selectUniqSavedTokensT :: Transaction (Vector (AssetName, POSIXTime))
selectUniqSavedTokensT = T.statement () selectUniqSavedTokens

-- *** From discarded

selectStaleDiscardedTokensT :: POSIXTime -> Transaction (Vector AssetName)
selectStaleDiscardedTokensT staleTime = T.statement staleTime selectStaleDiscardedTokens

selectAllDiscardedTokensT :: Transaction (Vector AssetName)
selectAllDiscardedTokensT = T.statement () selectAllDiscardedTokens

-- ** Insert

-- *** Into encoins

insertTokenT :: AssetName
  -> EncryptedSecret
  -> POSIXTime
  -> Transaction Int32
insertTokenT assetName encryptedSecret createTime =
  T.statement (assetName, encryptedSecret, createTime) insertToken

-- *** Into discarded

insertDiscardedTokensT :: Vector (AssetName, POSIXTime)
  -> Transaction ()
insertDiscardedTokensT discardedTokens =
  T.statement discardedTokens insertDiscardedTokens

-- ** Delete

-- *** From encoins

deleteTokensByNameT :: AssetName -> Transaction (Vector (Text, Text))
deleteTokensByNameT assetName =
  T.statement assetName deleteTokensByName

-- *** From discarded

deleteDiscardedTokensT :: Vector AssetName -> Transaction ()
deleteDiscardedTokensT discardedTokens =
  T.statement discardedTokens deleteDiscardedTokens

deleteDiscardedLinksT :: Vector AssetName -> Transaction ()
deleteDiscardedLinksT discardedTokens =
  T.statement discardedTokens deleteDiscardedLinks
