{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Encoins.Relay.Server.Api where

import qualified CSL
import           Cardano.Server.Endpoints.Funds     (Funds, FundsReqBody)
import           Cardano.Server.Endpoints.Tx.Submit (SubmitTxReqBody)
import           Cardano.Server.Error               (Throws)
import           Cardano.Server.Error.Servant       (Throwing)
import           Data.Text                          (Text)
import           Data.Type.Equality                 ((:~:) (..))
import           ENCOINS.Core.OnChain               (EncoinsRedeemer)
import           ENCOINS.Core.OffChain              (EncoinsMode (..))
import           Encoins.Relay.Server.Server        (EncoinsApi)
import           Encoins.Relay.Server.Status        (EncoinsStatusReqBody, EncoinsStatusResult)
import           Ledger                             (Address)
import           Servant                            (Get, Post, JSON, NoContent, Post, ReqBody, type (:<|>), type (:>))

type Api
    =    "ping"     
      :> Get '[JSON] NoContent

    :<|> "funds"    
      :> ReqBody '[JSON] FundsReqBody 
      :> Get '[JSON] Funds

    :<|> "newTx"    
      :> ReqBody '[JSON] (Either (Address, CSL.Value, Address) (EncoinsRedeemer, EncoinsMode), CSL.TransactionInputs) 
      :> Post '[JSON] (Text, Text)

    :<|> "submitTx" 
      :> ReqBody '[JSON] SubmitTxReqBody 
      :> Post '[JSON] NoContent

    :<|> "serverTx" 
      :> ReqBody '[JSON] (Either (Address, CSL.Value, Address) (EncoinsRedeemer, EncoinsMode), CSL.TransactionInputs) 
      :> Post '[JSON] NoContent

    :<|> "status"   
      :> ReqBody '[JSON] EncoinsStatusReqBody 
      :> Post '[JSON] EncoinsStatusResult

thisApiIsActual :: Api :~: EraseErrors EncoinsApi
thisApiIsActual = Refl

type family EraseErrors a where
    EraseErrors (a        :<|> b) = EraseErrors a :<|> EraseErrors b
    EraseErrors (Throws   _ :> b) = EraseErrors b
    EraseErrors (Throwing _ :> b) = EraseErrors b
    EraseErrors (a          :> b) = a :> EraseErrors b
    EraseErrors  a                = a