{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Apps.Save.Database.Migration where

import           Control.Monad              (void)
import           Hasql.Migration
import           Hasql.Migration.Util       (existsTable)
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as Tx
import qualified Hasql.Transaction.Sessions as Tx

migration :: P.Pool -> IO (Either P.UsageError (Either String (Maybe MigrationError)))
migration pool = do
  void $ runTx pool $ runMigration $ MigrationInitialization
  migrationFile <- loadMigrationFromFile "encoins.sql" "schema/encoins.sql"
  r <- runTx pool $ migrateToEncoinsSchema migrationFile
  pure r

migrateToEncoinsSchema :: MigrationCommand -> Tx.Transaction (Either String (Maybe MigrationError))
migrateToEncoinsSchema migrationFile = do
  exist <- existsTable "encoins"
  if exist then pure $ Left "encoins table exists" else Right <$> runMigration migrationFile

runTx :: P.Pool -> Tx.Transaction a -> IO (Either P.UsageError a)
runTx pool act = P.use pool $ Tx.transaction Tx.ReadCommitted Tx.Write act
