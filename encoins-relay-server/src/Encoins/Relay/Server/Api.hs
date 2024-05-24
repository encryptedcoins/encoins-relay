{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Encoins.Relay.Server.Api where

import qualified CSL
import           Encoins.Common.Version                 (AppVersion)
import           Encoins.Relay.Server.Endpoint.Status   (EncoinsStatusReqBody, EncoinsStatusResult)
import           Encoins.Relay.Server.Internal          (EncoinsServerInput)
import           Encoins.Relay.Server.Server            (EncoinsApi)

import           Data.Text                              (Text)
import           Data.Type.Equality                     ((:~:) (..))
import           Encoins.Relay.Common.Error.Servant     (Throwing, Throws)
import           Encoins.Relay.Server.Endpoint.SubmitTx (SubmitTxReqBody)
import           Servant                                (Get, JSON, NoContent, Post, ReqBody, type (:<|>), type (:>))

type Api
    =    "ping"
      :> Get '[JSON] NoContent

    :<|> "utxos"
      :> ReqBody '[JSON] Text
      :> Get '[JSON] CSL.TransactionUnspentOutputs

    :<|> "newTx"
      :> ReqBody '[JSON] (EncoinsServerInput, CSL.TransactionInputs)
      :> Post '[JSON] (Text, Text)

    :<|> "serverTx"
      :> ReqBody '[JSON] (EncoinsServerInput, CSL.TransactionInputs)
      :> Post '[JSON] NoContent

    :<|> "submitTx"
      :> ReqBody '[JSON] SubmitTxReqBody
      :> Post '[JSON] NoContent

    :<|> "status"
      :> ReqBody '[JSON] EncoinsStatusReqBody
      :> Post '[JSON] EncoinsStatusResult

    :<|> "version"
      :> Get '[JSON] AppVersion

thisApiIsActual :: Api :~: EraseErrors EncoinsApi
thisApiIsActual = Refl

type family EraseErrors a where
    EraseErrors (a        :<|> b) = EraseErrors a :<|> EraseErrors b
    EraseErrors (Throws   _ :> b) = EraseErrors b
    EraseErrors (Throwing _ :> b) = EraseErrors b
    EraseErrors (a          :> b) = a :> EraseErrors b
    EraseErrors  a                = a
