{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Encoins.Relay.Apps.Save.Database
    ( dbExe
    ) where

import Data.Text (Text)
import Data.Functor.Contravariant
import Data.Int
import Contravariant.Extras.Contrazip
import Data.Vector (Vector)
import Text.Pretty.Simple
import Data.UUID
import Data.UUID.V4


import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session (Session)
import qualified Hasql.Session as S
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction.Sessions as TS
import qualified Hasql.Transaction as T

dbExe :: IO ()
dbExe = do
  Right connection <- Connection.acquire connectionSettings

  pPrintString "Encoins..."
  tokens <- S.run getTokensSession connection
  pPrint tokens
  pPrintString "Insert encoins..."
  -- assetName <- toText <$> nextRandom
  secret <- toText <$> nextRandom
  -- result <- S.run (insertTokenSession assetName secret) connection
  result <- S.run (insertTokenSession "assetname" secret) connection
  pPrintString "Inserted id:"
  pPrint result
  pPrintString "Encoins..."
  tokens <- S.run getTokensSession connection
  pPrint tokens
  -- tokensByName <- S.run (getTokensByNameSession assetName) connection
  -- pPrint tokensByName
  pPrintString "Deleting encoins..."
  deletedTokens <- S.run (deleteTokensByNameSession "assetname") connection
  pPrintString "Deleted id:"
  pPrint deletedTokens
  pPrintString "Encoins..."
  tokens <- S.run getTokensSession connection
  pPrint tokens


  where
    connectionSettings = Connection.settings "127.0.0.1" 5432 "postgres" "" "encoins"


-- * Sessions
--
-- Session is an abstraction over the database connection and all possible errors.
-- It is used to execute statements.
-- It is composable and has a Monad instance.

insertTokenSession :: Text -> Text -> Session Int32
insertTokenSession assetName encryptedSecret = TS.transaction TS.Serializable TS.Write $
  insertTokenT assetName encryptedSecret

getTokensByNameSession :: Text -> Session (Vector (Text, Text))
getTokensByNameSession assetName = TS.transaction TS.ReadCommitted TS.Read $
  getTokensByNameT assetName

getTokensSession :: Session (Vector (Text, Text))
getTokensSession = TS.transaction TS.ReadCommitted TS.Read $
  getTokensT

deleteTokensByNameSession :: Text -> Session (Vector (Text, Text))
deleteTokensByNameSession assetName = TS.transaction TS.Serializable TS.Write $
  deleteTokensByNameT assetName

-- * Transaction

insertTokenT :: Text -> Text -> Transaction Int32
insertTokenT assetName encryptedSecret =
  T.statement (assetName, encryptedSecret) insertToken

getTokensByNameT :: Text -> Transaction (Vector (Text, Text))
getTokensByNameT assetName =
  T.statement assetName getTokensByName

getTokensT :: Transaction (Vector (Text, Text))
getTokensT =
  T.statement () getTokens

deleteTokensByNameT :: Text -> Transaction (Vector (Text, Text))
deleteTokensByNameT assetName =
  T.statement assetName deleteTokensByName

-- * Statements
--
-- Statement is a definition of an individual SQL-statement,
-- accompanied by a specification of how to encode its parameters and
-- decode its result.

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
