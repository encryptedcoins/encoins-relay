{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Server.Endpoints.Balance where

import           Control.Monad.Catch    (Exception, handle, throwM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson             (ToJSON)
import           Data.Text              (Text)
import qualified Data.Map               as M
import           GHC.Generics           (Generic)
import           IO.ChainIndex          (getUtxosAt)
import           Ledger                 (ChainIndexTxOut(..))
import           Plutus.V2.Ledger.Api   (Address, CurrencySymbol, TokenName, TxOutRef, Value(..))
import qualified PlutusTx.AssocMap      as PAM
import           Servant                ((:>), StdMethod(GET), JSON, respond, HasStatus,
                                         ReqBody, StatusOf, WithStatus, Union, UVerb)
import           Server.Internal        (AppM, HasServer(..))
import           Utils.Address          (bech32ToAddress)
import           Utils.Logger           (logMsg)
import           Utils.Servant          (respondWithStatus)

type BalanceApi = "relayRequestBalance"
               :> ReqBody '[JSON] Text
               :> UVerb 'GET '[JSON] BalanceApiResult

type BalanceApiResult = '[Balance, WithStatus 400 Text]

newtype Balance = Balance [(TokenName, TxOutRef)]
    deriving (Show, Generic)
    deriving newtype ToJSON

instance HasStatus Balance where
    type StatusOf Balance = 200

data BalanceError
    = UnparsableAddress
    deriving (Show, Exception)

balanceHandler :: forall s. HasServer s => Text -> AppM s (Union BalanceApiResult)
balanceHandler addrBech32 = handle balanceErrorHandler $ do
    logMsg $ "New balance request received:\n" <> addrBech32
    addr <- maybe (throwM UnparsableAddress) pure $ bech32ToAddress addrBech32
    cs   <- getCurrencySymbol @s
    respond =<< getBalance cs addr

balanceErrorHandler :: BalanceError -> AppM s (Union BalanceApiResult)
balanceErrorHandler = \case

    UnparsableAddress -> respondWithStatus @400
        "Incorrect wallet address."

getBalance :: MonadIO m => CurrencySymbol -> Address -> m Balance
getBalance cs addr = do
        coins <- liftIO $ M.toList . M.map getNames <$> getUtxosAt addr
        pure $ Balance $ concatMap (\(ref, names) -> zip names (repeat ref)) coins
    where
        getNames = maybe [] PAM.keys . PAM.lookup cs . getValue . _ciTxOutValue . fst