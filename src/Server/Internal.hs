module Server.Internal where

import Common.Tokens
import Control.Monad.Reader
import Data.IORef
import Data.Sequence
import Servant

type AppM = ReaderT Env Handler

type Queue = Seq Tokens

type Ref = IORef Queue

newtype Env = Env
    { envRef :: Ref
    }

getRef :: AppM Ref
getRef = asks envRef

logDebug :: MonadIO m => String -> m ()
logDebug = liftIO . putStrLn