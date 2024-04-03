{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Apps.Cloud.PostgreSQL.Query where

import           Encoins.Relay.Apps.Cloud.Types

import           Contravariant.Extras.Contrazip
import           Data.Functor.Contravariant     (contramap, (>$<))
import           Data.Int
import           Data.Text                      (Text)
import           Data.Time.Clock.POSIX          (POSIXTime)
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.Session                  (Session)
import           Hasql.Statement                (Statement (..))
import           Hasql.Transaction              (Transaction)
import qualified Hasql.Transaction              as T
import qualified Hasql.Transaction.Sessions     as TS

import qualified Control.Foldl                  as F
import qualified Hasql.CursorQuery              as CQ
import qualified Hasql.CursorQuery.Sessions     as CQS

-- * Sessions

insertTokenS :: AssetName
  -> EncryptedSecret
  -> POSIXTime
  -> Session Int32
insertTokenS assetName encryptedSecret createTime = TS.transaction TS.Serializable TS.Write $
  insertTokenT assetName encryptedSecret createTime

getTokensByNameS :: AssetName -> Session (Vector (Text, Text))
getTokensByNameS assetName = TS.transaction TS.ReadCommitted TS.Read $
  getTokensByNameT assetName

getIdOfNameSecretS :: AssetName
  -> EncryptedSecret
  -> Session (Maybe Int32)
getIdOfNameSecretS assetName encryptedSecret = TS.transaction TS.ReadCommitted TS.Read $
  getIdOfNameSecretT assetName encryptedSecret

getTokensS :: Session (Vector (Text, Text))
getTokensS = TS.transaction TS.ReadCommitted TS.Read $
  getTokensT

deleteTokensByNameS :: AssetName -> Session (Vector (Text, Text))
deleteTokensByNameS assetName = TS.transaction TS.ReadCommitted TS.Write $
  deleteTokensByNameT assetName

insertOnAbsentS :: AssetName -> EncryptedSecret -> POSIXTime -> Session Int32
insertOnAbsentS assetName encryptedSecret createTime =
  TS.transaction TS.Serializable TS.Write $ do
    mId <- getIdOfNameSecretT assetName encryptedSecret
    case mId of
      Nothing -> insertTokenT assetName encryptedSecret createTime
      Just i  -> pure i

countRowsS :: Session Int
countRowsS = CQS.cursorQuery () countRows

getDiscardedTokensS :: Session (Vector (AssetName, POSIXTime))
getDiscardedTokensS = TS.transaction TS.ReadCommitted TS.Read $
  getDiscardedTokensT

insertDiscardedTokensS :: Vector (AssetName, POSIXTime) -> Session ()
insertDiscardedTokensS = TS.transaction TS.Serializable TS.Write .
  insertDiscardedTokensT

-- * Transaction

insertTokenT :: AssetName
  -> EncryptedSecret
  -> POSIXTime
  -> Transaction Int32
insertTokenT assetName encryptedSecret createTime =
  T.statement (assetName, encryptedSecret, createTime) insertToken

getTokensByNameT :: AssetName -> Transaction (Vector (Text, Text))
getTokensByNameT assetName = T.statement assetName getTokensByName

getIdOfNameSecretT :: AssetName
  -> EncryptedSecret
  -> Transaction (Maybe Int32)
getIdOfNameSecretT assetName encryptedSecret =
  T.statement (assetName, encryptedSecret) getIdOfNameSecret

getTokensT :: Transaction (Vector (Text, Text))
getTokensT = T.statement () getTokens

deleteTokensByNameT :: AssetName -> Transaction (Vector (Text, Text))
deleteTokensByNameT assetName =
  T.statement assetName deleteTokensByName

getDiscardedTokensT :: Transaction (Vector (AssetName, POSIXTime))
getDiscardedTokensT = T.statement () getDiscardedTokens

insertDiscardedTokensT :: Vector (AssetName, POSIXTime)
  -> Transaction ()
insertDiscardedTokensT discardedTokens =
  T.statement discardedTokens insertDiscardedTokens

-- * Statements

insertToken :: Statement (AssetName, EncryptedSecret, POSIXTime) Int32
insertToken = let
  sql =
    "insert into encoins (asset_name, encrypted_secret, save_time) \
    \values ($1, $2, $3) \
    \returning id"
  encoder =
    contrazip3
      (contramap getAssetName $ E.param (E.nonNullable E.text))
      (contramap getEncryptedSecret $ E.param (E.nonNullable E.text))
      (contramap truncate (E.param (E.nonNullable E.int8 )))
  decoder =
    D.singleRow ((D.column . D.nonNullable) D.int4)
  in Statement sql encoder decoder True

getTokensByName :: Statement AssetName (Vector (Text, Text))
getTokensByName = let
  sql =
    "select asset_name, encrypted_secret \
    \from encoins \
    \where asset_name = $1"
  encoder =
    contramap getAssetName $ E.param (E.nonNullable E.text)
  decoder =
    D.rowVector $
      (,) <$>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.text)
  in Statement sql encoder decoder True

getIdOfNameSecret :: Statement (AssetName, EncryptedSecret) (Maybe Int32)
getIdOfNameSecret = let
  sql =
    "select id \
    \from encoins \
    \where asset_name = $1 and encrypted_secret = $2"
  encoder =
    contrazip2
      (contramap getAssetName $ E.param (E.nonNullable E.text))
      (contramap getEncryptedSecret $ E.param (E.nonNullable E.text))
  decoder =
    D.rowMaybe $ D.column (D.nonNullable D.int4)
  in Statement sql encoder decoder True

getTokens :: Statement () (Vector (Text, Text))
getTokens = let
  sql =
    "SELECT asset_name, encrypted_secret \
    \FROM encoins"
  encoder = E.noParams
  decoder =
    D.rowVector $
      (,) <$>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.text)
  in Statement sql encoder decoder False

deleteTokensByName :: Statement AssetName (Vector (Text, Text))
deleteTokensByName = let
  sql =
    "DELETE FROM encoins \
    \WHERE asset_name = $1 \
    \RETURNING asset_name, encrypted_secret"
  encoder =
    contramap getAssetName $ E.param (E.nonNullable E.text)
  decoder =
    D.rowVector $
      (,) <$>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.text)
  in Statement sql encoder decoder True

countRows :: CQ.CursorQuery () Int
countRows =
  CQ.cursorQuery sql encoder decoder CQ.batchSize_10
  where
    sql = "SELECT asset_name FROM encoins"
    encoder = E.noParams
    decoder = CQ.reducingDecoder rowDecoder fold
      where
        rowDecoder = D.column (D.nonNullable D.text)
        fold = F.length

getDiscardedTokens :: Statement () (Vector (AssetName, POSIXTime))
getDiscardedTokens = let
  sql =
    "SELECT DISTINCT asset_name, save_time  \
    \FROM encoins"
  encoder = E.noParams
  decoder =
    D.rowVector $
      (,) <$>
        fmap MkAssetName (D.column (D.nonNullable D.text)) <*>
        fmap fromIntegral (D.column (D.nonNullable D.int8))
  in Statement sql encoder decoder False

insertDiscardedTokens :: Statement (Vector (AssetName, POSIXTime)) ()
insertDiscardedTokens = let
  sql =
    "insert into discarded (asset_name, discard_time) \
    \select * from unnest ($1, $2) \
    \ON CONFLICT (asset_name) DO NOTHING;"
  encoder = V.unzip >$<
    (contrazip2
      (E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable $ contramap getAssetName E.text)
      (E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable $ contramap truncate E.int8)
    )
  decoder = D.noResult
  in Statement sql encoder decoder True


{-
delete :: [PayloadId] -> Session ()
delete xs = do
  let theQuery = [here|
        DELETE FROM payloads
        WHERE id = ANY($1)
        |]

      encoder = E.param
              $ E.nonNullable
              $ E.foldableArray
              $ E.nonNullable payloadIdEncoder

  statement xs $ Statement theQuery encoder D.noResult True
-}