{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Client.Internal where

import           Data.Kind            (Type)
import           Control.Monad.Reader (asks, MonadIO, MonadReader, ReaderT(..))
import           IO.Wallet            (HasWallet(..), RestoreWallet)
import           Options.Applicative  (Parser)
import qualified Server.Internal      as Server
import           Utils.Logger         (HasLogger(..))

newtype ClientM s a = ClientM { unClientM :: ReaderT (Env s) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (Env s), MonadIO)

runClientM :: Server.AuxiliaryEnvOf s -> RestoreWallet -> ClientM s a -> IO a
runClientM aEnv wallet = flip runReaderT (Env aEnv wallet) . unClientM

data Env s = Env
    { envAuxiliary :: Server.AuxiliaryEnvOf s
    , envWallet    :: RestoreWallet
    }

instance HasLogger (ClientM s) where
    loggerFilePath = "client.log"

instance HasWallet (ClientM s) where
    getRestoreWallet = asks envWallet

class ( Server.HasServer c
      , Show (RequestPieceOf c)
      , Eq (RequestPieceOf c)
      ) => HasClient c where

    type RequestPieceOf c :: Type

    parseRequestPiece :: Parser (RequestPieceOf c)

    genRequestPiece :: IO (RequestPieceOf c)

    -- here ClientM c () are some additional actions, that would be executed
    -- on successful response
    mkRedeemer :: ClientRequestOf c -> ClientM c (ClientM c (), Server.RedeemerOf c)

type ClientRequestOf s = [RequestPieceOf s]