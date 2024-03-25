{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Apps.Cloud.PostgreSQL.Query where

import           Encoins.Relay.Apps.Cloud.Types

import           Contravariant.Extras.Contrazip
import           Data.Int
import           Data.Text                      (Text)
import           Data.Time.Clock.POSIX          (POSIXTime)
import           Data.Vector                    (Vector)
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.Session                  (Session)
import           Hasql.Statement                (Statement (..))
import           Hasql.Transaction              (Transaction)
import qualified Hasql.Transaction              as T
import qualified Hasql.Transaction.Sessions     as TS
import Data.Functor.Contravariant (contramap)

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

getTokenNumberS :: Session Int32
getTokenNumberS = TS.transaction TS.ReadCommitted TS.Read $
  getTokenNumberT

-- * Transaction

insertTokenT :: AssetName
  -> EncryptedSecret
  -> POSIXTime
  -> Transaction Int32
insertTokenT assetName encryptedSecret createTime =
  T.statement (assetName, encryptedSecret, createTime) insertToken

getTokensByNameT :: AssetName -> Transaction (Vector (Text, Text))
getTokensByNameT assetName =
  T.statement assetName getTokensByName

getIdOfNameSecretT :: AssetName
  -> EncryptedSecret
  -> Transaction (Maybe Int32)
getIdOfNameSecretT assetName encryptedSecret =
  T.statement (assetName, encryptedSecret) getIdOfNameSecret

getTokensT :: Transaction (Vector (Text, Text))
getTokensT =
  T.statement () getTokens

deleteTokensByNameT :: AssetName -> Transaction (Vector (Text, Text))
deleteTokensByNameT assetName =
  T.statement assetName deleteTokensByName

getTokenNumberT :: Transaction Int32
getTokenNumberT =
  T.statement () getTokenNumber

-- * Statements

insertToken :: Statement (AssetName, EncryptedSecret, POSIXTime) Int32
insertToken = let
  sql =
    "insert into encoins (asset_name, encrypted_secret) \
    \values ($1, $2) \
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
    "select asset_name, encrypted_secret \
    \from encoins"
  encoder = E.noParams
  decoder =
    D.rowVector $
      (,) <$>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.text)
  in Statement sql encoder decoder True

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

getTokenNumber :: Statement () Int32
getTokenNumber = let
  sql =
    "SELECT COUNT(*) \
    \FROM encoins"
  encoder = E.noParams
  decoder = D.foldlRows
    (\acc _ -> acc + 1)
    0
    (D.column (D.nonNullable D.int4))
  in Statement sql encoder decoder True