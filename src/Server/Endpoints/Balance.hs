{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Server.Endpoints.Balance where

import           Utils.Logger           (logMsg)
import           Control.Monad.Catch    (Exception, handle, throwM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader   (asks)
import           Data.Aeson             (ToJSON)
import           Data.Text              (Text)
import qualified Data.Map               as M
import           ENCOINS.Core.OffChain  (encoinsSymbol, beaconCurrencySymbol)
import           GHC.Generics           (Generic)
import           IO.ChainIndex          (getUtxosAt)
import           Ledger                 (ChainIndexTxOut(..))
import           Plutus.V2.Ledger.Api   (Address, CurrencySymbol, TokenName, TxOutRef, Value(..))
import qualified PlutusTx.AssocMap      as PAM
import           Servant                ((:>), StdMethod(GET), JSON, respond, HasStatus,
                                         ReqBody, StatusOf, WithStatus, Union, UVerb)
import           Server.Internal        (AppM, envBeaconRef, respondWithStatus)
import           Utils.Address          (bech32ToAddress)

type BalanceApi = "relayRequestBalance"
               :> ReqBody '[JSON] Text
               :> UVerb 'GET '[JSON] BalanceApiResult

type BalanceApiResult = '[Balance, WithStatus 400 Text]
newtype Balance = Balance [(TokenName, TxOutRef)]
    deriving Generic
    deriving newtype ToJSON

instance HasStatus Balance where
    type StatusOf Balance = 200

data BalanceError
    = UnparsableAddress
    deriving (Show, Exception)

balanceHandler :: Text -> AppM (Union BalanceApiResult)
balanceHandler addrBech32 = handle balanceErrorHandler $ do
    logMsg $ "New balance request received:\n" <> addrBech32
    addr <- maybe (throwM UnparsableAddress) pure $ bech32ToAddress addrBech32
    ecs  <- encoinsSymbol . beaconCurrencySymbol <$> asks envBeaconRef
    respond =<< getBalance ecs addr

balanceErrorHandler :: BalanceError -> AppM (Union BalanceApiResult)
balanceErrorHandler = \case

    UnparsableAddress -> respondWithStatus @400
        "Incorrect wallet address."

getBalance :: MonadIO m => CurrencySymbol -> Address -> m Balance
getBalance ecs addr = do
    encoins <- liftIO $ M.toList . M.map getNames <$> getUtxosAt addr
    pure $ Balance $ concatMap (\(ref, names) -> zip names (repeat ref)) $ encoins
  where
    getNames = maybe [] PAM.keys . PAM.lookup ecs . getValue . _ciTxOutValue . fst