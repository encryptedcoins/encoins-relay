{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Encoins.Relay.Common.Tx.Input where

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default                     (Default (..))
import           GHC.Generics                     (Generic)
import           Ledger                           (TxOutRef)
import           Ledger.Address                   (Address)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)

data InputContext = InputContextServer
    {
        inputUTXO          :: MapUTXO           -- Unspent outputs for satisfying transaction constraints
    }
    | InputContextClient
    {
        inputUTXO          :: MapUTXO,    -- Unspent outputs for satisfying transaction constraints
        inputWalletUTXO    :: MapUTXO,    -- Unspent outputs for balancing an unbalanced transaction
        inputCollateral    :: TxOutRef,   -- Reference of a collateral input
        inputChangeAddress :: Address     -- Address for the change
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Default InputContext where
    def = InputContextServer mempty
