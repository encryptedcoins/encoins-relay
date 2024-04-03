{-# LANGUAGE OverloadedStrings #-}

module Encoins.Relay.Apps.Cloud.PostgreSQL.Migration where

import           Control.Exception.Safe     (throwString)
import           Control.Monad              (forM_)
import qualified Hasql.Migration            as M
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as Tx
import qualified Hasql.Transaction.Sessions as Tx
import           Text.Pretty.Simple         (pPrint)

runTx :: P.Pool -> Tx.Transaction a -> IO (Either P.UsageError a)
runTx pool tx = P.use pool $ Tx.transaction Tx.ReadCommitted Tx.Write tx

migration :: P.Pool -> FilePath -> IO ()
migration pool path = do
  oldMigs <- runTx pool M.getMigrations
  pPrint "Previous migrations:"
  pPrint oldMigs
  commands <- M.loadMigrationsFromDirectory path
  let transactions =
        fmap M.runMigration $
          M.MigrationInitialization : commands
  forM_ transactions $ \transaction -> do
    res <- runTx pool transaction
    case res of
      Left e -> do
        pPrint e
        throwString "Query error"
      Right (Just e) -> do
        pPrint e
        throwString "Migration error"
      Right Nothing  -> pure ()
