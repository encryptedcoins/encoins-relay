{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Apps.Save.Database.Migration where

import           Hasql.Connection
import           Hasql.Migration
import           Hasql.Migration.Util       (existsTable)
import           Hasql.Session              (QueryError, run)
import qualified Hasql.Transaction          as Tx
import qualified Hasql.Transaction.Sessions as Tx

import           Text.Pretty.Simple

migration :: Connection -> IO (Either QueryError (Either String (Maybe MigrationError)))
migration con = do
  i <- runTx con $ runMigration $ MigrationInitialization
  pPrint i
  migrationFile <- loadMigrationFromFile "encoins.sql" "schema/encoins.sql"
  r <- runTx con $ migrateToEncoinsSchema migrationFile
  pPrint r
  pure r


migrateToEncoinsSchema :: MigrationCommand -> Tx.Transaction (Either String (Maybe MigrationError))
migrateToEncoinsSchema migrationFile = do
  exist <- existsTable "encoins"
  if exist then pure $ Left "encoins table exists" else Right <$> runMigration migrationFile


runTx :: Connection -> Tx.Transaction a -> IO (Either QueryError a)
runTx con act = do
    run (Tx.transaction Tx.ReadCommitted Tx.Write act) con
