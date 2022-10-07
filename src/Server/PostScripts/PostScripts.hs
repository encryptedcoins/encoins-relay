{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module Server.PostScripts.PostScripts where

import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Reader             (ask)
import           Common.Logger                    (HasLogger(..))
import qualified Ledger.Ada                       as Ada
import           Ledger.Typed.Scripts             (Any)
import           Scripts.Constraints              (postMintingPolicyTx)
import           Server.PostScripts.MintingPolicy (serverMintingPolicy)
import           Server.ServerTx

newtype PostScriptsM a = PostScriptsM { unPostScriptsM :: IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance HasLogger PostScriptsM where
    loggerFilePath = "server.log"

postScripts :: IO ()
postScripts = unPostScriptsM $ mkTxWithConstraints @Any $ do
    TxEnv{..} <- ask
    let c = postMintingPolicyTx
            txEnvWalletAddr
            serverMintingPolicy
            (Nothing :: Maybe ())
            (Ada.adaValueOf 10)
    pure [c]
