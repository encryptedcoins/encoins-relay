{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module EncoinsServer.Server where

import           Control.Exception                        (Exception, throw)
import           Control.Monad                            (void)
import           Data.Aeson                               (decode)
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.Default                             (def)
import           Data.FileEmbed                           (embedFile)
import           Data.Maybe                               (fromJust)
import           Ledger                                   (Address, TxId (TxId), TxOutRef (..))
import           PlutusTx.Prelude                         (BuiltinByteString, toBuiltin)
import           Text.Hex                                 (Text, decodeHex)

import           Cardano.Server.Config                    (decodeOrErrorFromFile)
import           Cardano.Server.Error                     (IsCardanoServerError (errMsg, errStatus), toEnvelope)
import           Cardano.Server.Input                     (InputContext (..))
import           Cardano.Server.Internal                  (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (..), ServerM,
                                                           getAuxillaryEnv)
import           Cardano.Server.Main                      (ServerApi)
import           Cardano.Server.Tx                        (mkTx)

import           CSL                                      (TransactionUnspentOutputs)

import           ENCOINS.Core.OffChain                    (beaconMintTx, beaconSendTx, encoinsTx)
import           ENCOINS.Core.OnChain                     (beaconCurrencySymbol, encoinsSymbol, stakingValidatorAddress)
import           ENCOINS.Core.V1.OffChain                 (EncoinsRedeemerWithData, postEncoinsPolicyTx, postStakingValidatorTx)
import           ENCOINS.Core.V1.OnChain                  (hashRedeemer)

import           CSL.Class                                (FromCSL (..))
import qualified Data.Map                                 as Map
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet                (getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder)
import           PlutusAppsExtra.Utils.Address            (bech32ToAddress)
import           PlutusAppsExtra.Utils.Crypto             (sign)

verifierPKH :: BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"

verifierPrvKey :: BuiltinByteString
verifierPrvKey = fromJust $ decode $ fromStrict $(embedFile "config/prvKey.json")

relayWalletAddress :: Address
relayWalletAddress = fromJust $ bech32ToAddress 
    "addr_test1qrejftzwv7lckpzx6z3wsv9hzvftf79elxpzjua05rtvl0z3ky8mdwfpnthjm0k39km55frq38en0kdc2g935zs0xhmqlckudm"

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress 
    "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

mkServerHandle :: IO (ServerHandle EncoinsApi)
mkServerHandle = do
    aEnv <- decodeOrErrorFromFile "txOutRef.json"
    pure $ ServerHandle
        Kupo
        aEnv
        getTrackedAddresses
        txBuilders
        (pure ())
        processRequest
        (const $ toEnvelope $ pure "Not implemented yet.")

type EncoinsApi = ServerApi
    (EncoinsRedeemerWithData, TransactionUnspentOutputs)
    EncoinsTxApiError
    ()
    '[]
    Text

type instance InputOf        EncoinsApi = EncoinsRedeemerWithData
type instance AuxillaryEnvOf EncoinsApi = TxOutRef

data EncoinsTxApiError
    = CorruptedExternalUTXOs
    | IncorrectVerifierSignature
    deriving (Show, Exception)

instance IsCardanoServerError EncoinsTxApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        CorruptedExternalUTXOs     -> "The request contained corrupted external UTXO data."
        IncorrectVerifierSignature -> "The request contained incorrect verifier signature."

serverSetup :: ServerM EncoinsApi ()
serverSetup = void $ do
    txOutRef <- getAuxillaryEnv
    utxos    <- getWalletUtxos
    -- -- Mint the beacon
    mkTx [] (InputContextServer utxos) [beaconMintTx txOutRef]
    utxos' <- getWalletUtxos
    -- Send it to the staking address
    mkTx [] (InputContextServer utxos') [beaconSendTx txOutRef verifierPKH]
    let encoinsParams = (beaconCurrencySymbol txOutRef, verifierPKH)
        stakingParams = encoinsSymbol encoinsParams
    -- Post the ENCOINS minting policy
    mkTx [] def [postEncoinsPolicyTx encoinsParams 15]
    -- Post the staking validator policy
    mkTx [] def [postStakingValidatorTx stakingParams 15]

getTrackedAddresses :: ServerM EncoinsApi [Address]
getTrackedAddresses = do
    txOutRef <- getAuxillaryEnv
    let symb = encoinsSymbol (beaconCurrencySymbol txOutRef, verifierPKH)
    return [stakingValidatorAddress symb, alwaysFalseValidatorAddress 15]

processRequest :: (EncoinsRedeemerWithData, TransactionUnspentOutputs)
    -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest (redWithData@(addr, _), utxosCSL) = do
    let utxos   = maybe (throw CorruptedExternalUTXOs) Map.fromList $ fromCSL utxosCSL
        context = InputContextClient utxos utxos
            (TxOutRef (TxId "") 0)
            addr
    return (redWithData, context)

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (addr, red@(par, input, proof, _)) = do
    bcs <- beaconCurrencySymbol <$> getAuxillaryEnv
    let red' = (par, input, proof, sign verifierPrvKey $ hashRedeemer red)
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) (bcs, verifierPKH) (addr, red')]