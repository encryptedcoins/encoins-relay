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

module Encoins.Relay.Server.Server where

import           CSL                                      (TransactionUnspentOutputs)
import           CSL.Class                                (FromCSL (..))
import           Cardano.Server.Config                    (decodeOrErrorFromFile)
import           Cardano.Server.Error                     (IsCardanoServerError (errMsg, errStatus), toEnvelope)
import           Cardano.Server.Input                     (InputContext (..))
import           Cardano.Server.Internal                  (AuxillaryEnvOf, InputOf, InputWithContext, ServerHandle (..), ServerM,
                                                           getAuxillaryEnv)
import           Cardano.Server.Main                      (ServerApi)
import           Cardano.Server.Tx                        (mkTx)
import           Control.Exception                        (Exception, throw)
import           Control.Monad                            (unless, void)
import           Control.Monad.Catch                      (MonadThrow (..))
import           Data.Aeson                               (decode, eitherDecode)
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.Default                             (def)
import           Data.FileEmbed                           (embedFile)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromJust, fromMaybe)
import           ENCOINS.BaseTypes                        (toGroupElement)
import           ENCOINS.Bulletproofs                     (verify)
import           ENCOINS.Bulletproofs.Types               (BulletproofSetup, Input (Input), parseBulletproofParams)
import           ENCOINS.Core.OffChain                    (beaconTx, encoinsTx, postEncoinsPolicyTx, postStakingValidatorTx,
                                                           stakeOwnerTx)
import           ENCOINS.Core.OnChain                     (EncoinsRedeemer, beaconCurrencySymbol, encoinsSymbol,
                                                           ledgerValidatorAddress)
import           ENCOINS.Core.V1.OffChain                 (EncoinsMode (..))
import           ENCOINS.Core.V1.OnChain                  (hashRedeemer)
import           Ledger                                   (Address, TxId (TxId), TxOutRef (..))
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndex (..))
import           PlutusAppsExtra.IO.Wallet                (getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder)
import           PlutusAppsExtra.Utils.Address            (bech32ToAddress)
import           PlutusAppsExtra.Utils.Crypto             (sign)
import           PlutusTx.Extra.ByteString                (ToBuiltinByteString (..))
import           PlutusTx.Prelude                         (BuiltinByteString, sha2_256, toBuiltin)
import           Text.Hex                                 (Text, decodeHex)

verifierPKH :: BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"

verifierPrvKey :: BuiltinByteString
verifierPrvKey = fromJust $ decode $ fromStrict $(embedFile "../config/prvKey.json")

relayWalletAddress :: Address
relayWalletAddress = fromJust $ bech32ToAddress
    "addr_test1qrejftzwv7lckpzx6z3wsv9hzvftf79elxpzjua05rtvl0z3ky8mdwfpnthjm0k39km55frq38en0kdc2g935zs0xhmqlckudm"

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress
    "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

referenceScriptSalt :: Integer
referenceScriptSalt = 20

bulletproofSetup :: BulletproofSetup
bulletproofSetup = either error id $ eitherDecode $ fromStrict $(embedFile "../config/bulletproof_setup.json")

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
    ((EncoinsRedeemer, EncoinsMode), TransactionUnspentOutputs)
    EncoinsTxApiError
    ()
    '[]
    Text

type instance InputOf        EncoinsApi = (EncoinsRedeemer, EncoinsMode)
type instance AuxillaryEnvOf EncoinsApi = (TxOutRef, TxOutRef)

data EncoinsTxApiError
    = CorruptedExternalUTXOs
    | IncorrectInput
    | IncorrectProof
    deriving (Show, Exception)

instance IsCardanoServerError EncoinsTxApiError where
    errStatus _ = toEnum 422
    errMsg = \case
        CorruptedExternalUTXOs -> "The request contained corrupted external UTXO data."
        IncorrectInput         -> "The request contained incorrect public input."
        IncorrectProof         -> "The request contained incorrect proof."

serverSetup :: ServerM EncoinsApi ()
serverSetup = void $ do
    (refStakeOwner, refBeacon) <- getAuxillaryEnv
    -- Mint the stake owner token
    utxos <- getWalletUtxos
    let utxos' = Map.delete refBeacon utxos
    mkTx [] (InputContextServer utxos') [stakeOwnerTx refStakeOwner]
    -- Mint and send the beacon
    utxos'' <- getWalletUtxos
    mkTx [] (InputContextServer utxos'') [beaconTx refBeacon verifierPKH refStakeOwner]
    -- Define scripts' parameters
    let encoinsParams = (beaconCurrencySymbol refBeacon, verifierPKH)
        ledgerParams  = encoinsSymbol encoinsParams
    -- Post the ENCOINS minting policy
    mkTx [] def [postEncoinsPolicyTx encoinsParams referenceScriptSalt]
    -- Post the staking validator policy
    mkTx [] def [postStakingValidatorTx ledgerParams referenceScriptSalt]

getTrackedAddresses :: ServerM EncoinsApi [Address]
getTrackedAddresses = do
    (refStakeOwner, refBeacon) <- getAuxillaryEnv
    let encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
        stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
    return [ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb), alwaysFalseValidatorAddress referenceScriptSalt]

processRequest :: (InputOf EncoinsApi, TransactionUnspentOutputs) -> ServerM EncoinsApi (InputWithContext EncoinsApi)
processRequest ((red@((_, addr), _, _, _), mode), utxosCSL) = do
    let utxos   = maybe (throw CorruptedExternalUTXOs) Map.fromList $ fromCSL utxosCSL
        context = case mode of
            WalletMode -> InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
            LedgerMode -> InputContextServer mempty
    return ((red, mode), context)

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (red@(par, input, proof, _), mode) = do
    (refStakeOwner, refBeacon) <- getAuxillaryEnv
    let beaconSymb = beaconCurrencySymbol refBeacon
        stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
        red' = (par, input, proof, sign verifierPrvKey $ hashRedeemer red)
        bp   = parseBulletproofParams $ sha2_256 $ toBytes par
        v    = fst input
        ins  = map (\(bs, p) -> Input (fromMaybe (throw IncorrectInput) $ toGroupElement bs) p) $ snd input
    unless (verify bulletproofSetup bp v ins proof) $ throwM IncorrectProof
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) (beaconSymb, verifierPKH) stakeOwnerSymb red' mode]