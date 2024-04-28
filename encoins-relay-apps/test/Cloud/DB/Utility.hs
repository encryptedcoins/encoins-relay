{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module Cloud.DB.Utility where

import           Control.Exception.Safe             (Exception, catch, throwIO,
                                                     throwString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString.Char8              (pack)
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (Last (..))
import           Data.Typeable
import qualified Database.Postgres.Temp             as Temp
import           Database.PostgreSQL.Simple.Options (Options (..))
import qualified Hasql.Connection                   as Conn
import qualified Hasql.Decoders                     as HD
import qualified Hasql.Pool                         as P
import           Hasql.Session                      (Session)
import qualified Hasql.Session                      as S
import qualified Hasql.Statement                    as HST
import           System.Exit                        (ExitCode (..))
import           Test.Hspec                         (Expectation, shouldReturn)
import           Test.Tasty                         (DependencyType (..),
                                                     TestTree, after,
                                                     defaultMain, withResource)
import           Test.Tasty.HUnit                   (testCase)

withDb :: ByteString -> (IO P.Pool -> TestTree) -> IO ()
withDb initSql testF = defaultMain $
  withResource (startupPostgres initSql) (teardownPostgres) $ \mkDb ->
    withResource (mkDb >>= allocatePool) (freePool) $ \pool ->
      testF pool

nextTest :: IO () -> IO () -> IO ()
nextTest test nextTest = catch test $ \e -> do
    if e == ExitSuccess
      then nextTest
      else pure ()

testSequential :: (Eq a, Show a)
  => IO P.Pool
  -> String
  -> String
  -> Session a
  -> Either P.UsageError a
  -> TestTree
testSequential pool depName name session expected
  = after AllSucceed depName
  $ testCase name
  $ testSession pool session expected

testSession :: (Eq a, Show a)
  => IO P.Pool
  -> Session a
  -> Either P.UsageError a
  -> Expectation
testSession pool session expected = do
  p <- pool
  P.use p session `shouldReturn` expected

data InitException
  = InitException S.QueryError
  | ConnectException Conn.ConnectionError
  | PostgresStartException Temp.StartError
  deriving (Show, Typeable)

instance Exception InitException

startupPostgres :: ByteString -> IO Temp.DB
startupPostgres init_script = startupPostgresInit script where
  script c = do
    S.run (S.sql init_script ) c >>= \case
      Right {} -> pure ()
      Left e   -> throwIO $ InitException e

startupPostgresInit :: (Conn.Connection -> IO ()) -> IO Temp.DB
startupPostgresInit run_init = do
  Temp.start >>= \case
    Left e -> throwIO $ PostgresStartException e
    Right db -> do
      c <- Conn.acquire (Temp.toConnectionString db) >>= \case
             Left e  -> throwIO $ ConnectException e
             Right c -> pure c
      run_init c
      pure db

-- | Teardown database and associated resources
teardownPostgres :: Temp.DB -> IO ()
teardownPostgres = Temp.stop

allocatePool :: Temp.DB -> IO P.Pool
allocatePool db = do
  settings <- toSettings $ Temp.toConnectionOptions db
  P.acquire 3 10 1_800 settings

toSettings :: Options -> IO Conn.Settings
toSettings options = do
  let fromOptionM f n = case getLast $ f options of
        Nothing -> throwString $ "Not found field " <> n
        Just o  -> pure o
  let fromOption f = pack $ fromMaybe "" $ getLast $ f options
  host' <- pack <$> fromOptionM host "host"
  port' <- fromIntegral <$> fromOptionM port "port"
  let user' = fromOption user
  let pass = fromOption password
  db <- pack <$> fromOptionM dbname "db"
  pure $ Conn.settings host' port' user' pass db

freePool :: P.Pool -> IO ()
freePool = P.release
