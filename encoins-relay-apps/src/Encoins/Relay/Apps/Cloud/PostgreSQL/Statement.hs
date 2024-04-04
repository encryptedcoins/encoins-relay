{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Encoins.Relay.Apps.Cloud.PostgreSQL.Statement where

import           Encoins.Relay.Apps.Cloud.Types

import           Contravariant.Extras.Contrazip
import qualified Control.Foldl                   as F
import           Data.Functor.Contravariant      (contramap, (>$<))
import           Data.Int
import           Data.String.Here.Uninterpolated (here)
import           Data.Text                       (Text)
import           Data.Time.Clock.POSIX           (POSIXTime)
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import qualified Hasql.CursorQuery               as CQ
import qualified Hasql.Decoders                  as D
import qualified Hasql.Encoders                  as E
import           Hasql.Statement                 (Statement (..))

-- * Select

-- ** From encoins

getTokensByName :: Statement AssetName (Vector (Text, Text))
getTokensByName = let
  sql =
    "SELECT asset_name, encrypted_secret \
    \FROM encoins \
    \WHERE asset_name = $1"
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
    "SELECT id \
    \FROM encoins \
    \WHERE asset_name = $1 AND encrypted_secret = $2"
  encoder =
    contrazip2
      (contramap getAssetName $ E.param (E.nonNullable E.text))
      (contramap getEncryptedSecret $ E.param (E.nonNullable E.text))
  decoder =
    D.rowMaybe $ D.column (D.nonNullable D.int4)
  in Statement sql encoder decoder True

getAllSavedTokens :: Statement () (Vector (Text, Text))
getAllSavedTokens = let
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

selectUniqSavedTokens :: Statement () (Vector (AssetName, POSIXTime))
selectUniqSavedTokens = let
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

-- ** From discarded

selectStaleDiscardedTokens :: Statement POSIXTime (Vector AssetName)
selectStaleDiscardedTokens = let
  sql =
    "SELECT asset_name \
    \FROM discarded \
    \WHERE discard_time < $1"
  encoder = contramap truncate (E.param (E.nonNullable E.int8 ))
  decoder = D.rowVector $ fmap MkAssetName (D.column (D.nonNullable D.text))
  in Statement sql encoder decoder True

selectAllDiscardedTokens :: Statement () (Vector AssetName)
selectAllDiscardedTokens = let
  sql =
    "SELECT asset_name  \
    \FROM discarded"
  encoder = E.noParams
  decoder = D.rowVector $ fmap MkAssetName $ D.column $ D.nonNullable D.text
  in Statement sql encoder decoder False

-- * Insert

-- ** Into encoins

insertToken :: Statement (AssetName, EncryptedSecret, POSIXTime) Int32
insertToken = let
  sql =
    "INSERT INTO encoins (asset_name, encrypted_secret, save_time) \
    \VALUES ($1, $2, $3) \
    \RETURNING id"
  encoder =
    contrazip3
      (contramap getAssetName $ E.param (E.nonNullable E.text))
      (contramap getEncryptedSecret $ E.param (E.nonNullable E.text))
      (contramap truncate (E.param (E.nonNullable E.int8 )))
  decoder =
    D.singleRow ((D.column . D.nonNullable) D.int4)
  in Statement sql encoder decoder True

-- ** Into discarded

insertDiscardedTokens :: Statement (Vector (AssetName, POSIXTime)) ()
insertDiscardedTokens = let
  sql =
    "INSERT INTO discarded (asset_name, discard_time) \
    \SELECT * FROM unnest ($1, $2) \
    \ON CONFLICT (asset_name) DO NOTHING;"
  encoder = V.unzip >$<
    (contrazip2
      (E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable $ contramap getAssetName E.text)
      (E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable $ contramap truncate E.int8)
    )
  decoder = D.noResult
  in Statement sql encoder decoder True

-- * Delete

-- ** From encoins

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

deleteDiscardedTokens :: Statement (Vector AssetName) ()
deleteDiscardedTokens = let
  sql = [here|
        DELETE FROM encoins
        WHERE asset_name IN (SELECT unnest($1))
        |]
  encoder =
      E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable $ contramap getAssetName E.text
  decoder = D.noResult
  in Statement sql encoder decoder True

-- ** From discarded

deleteDiscardedTokenLinks :: Statement (Vector AssetName) ()
deleteDiscardedTokenLinks = let
  sql = [here|
        DELETE FROM discarded
        WHERE asset_name IN (SELECT unnest($1))
        |]
  encoder =
      E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable $ contramap getAssetName E.text
  decoder = D.noResult
  in Statement sql encoder decoder True
