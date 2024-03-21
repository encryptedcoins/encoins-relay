{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Encoins.Relay.Apps.Save.Database.Query where

import           Contravariant.Extras.Contrazip
import           Data.Int
import           Data.Text                      (Text)
import           Data.Vector                    (Vector)
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.Session                  (Session)
import           Hasql.Statement                (Statement (..))
import           Hasql.Transaction              (Transaction)
import qualified Hasql.Transaction              as T
import qualified Hasql.Transaction.Sessions     as TS

-- * Sessions

insertTokenS :: Text -> Text -> Session Int32
insertTokenS assetName encryptedSecret = TS.transaction TS.Serializable TS.Write $
  insertTokenT assetName encryptedSecret

getTokensByNameS :: Text -> Session (Vector (Text, Text))
getTokensByNameS assetName = TS.transaction TS.ReadCommitted TS.Read $
  getTokensByNameT assetName

getIdOfNameSecretS :: Text -> Text -> Session (Maybe Int32)
getIdOfNameSecretS assetName encryptedSecret = TS.transaction TS.ReadCommitted TS.Read $
  getIdOfNameSecretT assetName encryptedSecret

getTokensS :: Session (Vector (Text, Text))
getTokensS = TS.transaction TS.ReadCommitted TS.Read $
  getTokensT

deleteTokensByNameS :: Text -> Session (Vector (Text, Text))
deleteTokensByNameS assetName = TS.transaction TS.ReadCommitted TS.Write $
  deleteTokensByNameT assetName

insertOnAbsentS :: Text -> Text -> Session Int32
insertOnAbsentS assetName encryptedSecret = TS.transaction TS.Serializable TS.Write $ do
  mId <- getIdOfNameSecretT assetName encryptedSecret
  case mId of
    Nothing -> insertTokenT assetName encryptedSecret
    Just i  -> pure i

-- * Transaction

insertTokenT :: Text -> Text -> Transaction Int32
insertTokenT assetName encryptedSecret =
  T.statement (assetName, encryptedSecret) insertToken

getTokensByNameT :: Text -> Transaction (Vector (Text, Text))
getTokensByNameT assetName =
  T.statement assetName getTokensByName

getIdOfNameSecretT :: Text -> Text -> Transaction (Maybe Int32)
getIdOfNameSecretT assetName encryptedSecret =
  T.statement (assetName, encryptedSecret) getIdOfNameSecret

getTokensT :: Transaction (Vector (Text, Text))
getTokensT =
  T.statement () getTokens

deleteTokensByNameT :: Text -> Transaction (Vector (Text, Text))
deleteTokensByNameT assetName =
  T.statement assetName deleteTokensByName

-- * Statements

insertToken :: Statement (Text, Text) Int32
insertToken = let
  sql =
    "insert into encoins (asset_name, encrypted_secret) \
    \values ($1, $2) \
    \returning id"
  encoder =
    contrazip2
      (E.param (E.nonNullable E.text))
      (E.param (E.nonNullable E.text))
  decoder =
    D.singleRow ((D.column . D.nonNullable) D.int4)
  in Statement sql encoder decoder True

getTokensByName :: Statement Text (Vector (Text, Text))
getTokensByName = let
  sql =
    "select asset_name, encrypted_secret \
    \from encoins \
    \where asset_name = $1"
  encoder =
    E.param (E.nonNullable E.text)
  decoder =
    D.rowVector $
      (,) <$>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.text)
  in Statement sql encoder decoder True

getIdOfNameSecret :: Statement (Text, Text) (Maybe Int32)
getIdOfNameSecret = let
  sql =
    "select id \
    \from encoins \
    \where asset_name = $1 and encrypted_secret = $2"
  encoder =
    contrazip2
      (E.param (E.nonNullable E.text))
      (E.param (E.nonNullable E.text))
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

deleteTokensByName :: Statement Text (Vector (Text, Text))
deleteTokensByName = let
  sql =
    "delete from encoins \
    \where asset_name = $1 \
    \returning asset_name, encrypted_secret"
  encoder =
    E.param (E.nonNullable E.text)
  decoder =
    D.rowVector $
      (,) <$>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.text)
  in Statement sql encoder decoder True
