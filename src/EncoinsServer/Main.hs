{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module EncoinsServer.Main where

import           Control.Exception                        (throw)
import           Control.Monad                            (void, unless)
import           Control.Monad.Catch                      (throwM)
import           Control.Monad.Reader                     (asks)
import           Data.Aeson                               (decode)
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.Default                             (def)
import           Data.FileEmbed                           (embedFile)
import           Data.Map                                 (fromList, delete)
import           Data.Maybe                               (fromJust, fromMaybe)
import           Ledger                                   (TxOutRef (..), TxId (..), Address)
import           PlutusTx.Prelude                         (toBuiltin, BuiltinByteString, sha2_256)
import           Text.Hex                                 (decodeHex)

import           Cardano.Server.Class                     (HasServer (..), Env (..))
import           Cardano.Server.Endpoints.Tx.Class        (HasTxEndpoints(..))
import           Cardano.Server.Error                     (IsCardanoServerError (..))
import           Cardano.Server.Input                     (InputContext(..))
import           Cardano.Server.Internal                  (runAppM)
import           Cardano.Server.Main                      (runServer)
import           Cardano.Server.Tx                        (mkTx)
import           CSL                                      (TransactionUnspentOutputs)
import           CSL.Class                                (fromCSL)
import           ENCOINS.BaseTypes                        (toGroupElement)
import           ENCOINS.Bulletproofs                     (BulletproofSetup, Input (..), verify, parseBulletproofParams)
import           ENCOINS.Core.OffChain                    (encoinsTx, stakeOwnerTx, beaconTx, postStakingValidatorTx)
import           ENCOINS.Core.V1.OffChain                 (postEncoinsPolicyTx, EncoinsMode (..))
import           ENCOINS.Core.V1.OnChain                  (hashRedeemer)
import           ENCOINS.Core.OnChain                     (beaconCurrencySymbol, encoinsSymbol, ledgerValidatorAddress, EncoinsRedeemer)
import           EncoinsServer.Opts                       (ServerMode(..), runWithOpts)
import           PlutusAppsExtra.IO.ChainIndex            (ChainIndex(..))
import           PlutusAppsExtra.IO.Wallet                (getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Utils.Address            (bech32ToAddress)
import           PlutusAppsExtra.Utils.Crypto             (sign)
import           PlutusTx.Extra.ByteString                (toBytes)

referenceScriptSalt :: Integer
referenceScriptSalt = 20

bulletproofSetup :: BulletproofSetup
bulletproofSetup = fromJust $ decode $ fromStrict $(embedFile "config/bulletproof_setup.json")

verifierPKH :: BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"

verifierPrvKey :: BuiltinByteString
verifierPrvKey = fromJust $ decode $ fromStrict $(embedFile "config/prvKey.json")

relayWalletAddress :: Address
relayWalletAddress = fromJust $ bech32ToAddress "addr_test1qrejftzwv7lckpzx6z3wsv9hzvftf79elxpzjua05rtvl0z3ky8mdwfpnthjm0k39km55frq38en0kdc2g935zs0xhmqlckudm"

treasuryWalletAddress :: Address
treasuryWalletAddress = fromJust $ bech32ToAddress "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

runEncoinsServer :: IO ()
runEncoinsServer = do
    mode <- runWithOpts
    case mode of
        Run   -> runServer @EncoinsServer
        Setup -> runAppM (serverSetup @EncoinsServer)

data EncoinsServer

instance HasServer EncoinsServer where

    type AuxiliaryEnvOf EncoinsServer = (TxOutRef, TxOutRef)

    type InputOf EncoinsServer = (EncoinsRedeemer, EncoinsMode)

    serverSetup = void $ do
        (refStakeOwner, refBeacon) <- asks envAuxiliary
        -- Mint the stake owner token
        utxos <- getWalletUtxos
        let utxos' = delete refBeacon utxos
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

    serverIdle = pure ()

    serverTrackedAddresses = do
        (refStakeOwner, refBeacon) <- asks envAuxiliary
        let encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
            stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
        return [ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb), alwaysFalseValidatorAddress referenceScriptSalt]

    defaultChainIndex = Kupo

instance HasTxEndpoints EncoinsServer where
    type TxApiRequestOf EncoinsServer = (InputOf EncoinsServer, TransactionUnspentOutputs)

    data TxEndpointsErrorOf EncoinsServer = CorruptedExternalUTXOs | IncorrectInput | IncorrectProof
        deriving (Show)

    txEndpointsProcessRequest ((red@((_, addr), _, _, _), mode), utxosCSL) = do
        let utxos   = maybe (throw CorruptedExternalUTXOs) fromList $ fromCSL utxosCSL
            context = case mode of
                WalletMode -> InputContextClient utxos utxos (TxOutRef (TxId "") 1) addr
                LedgerMode -> InputContextServer mempty
        return ((red, mode), context)

    txEndpointsTxBuilders (red@(par, input, proof, _), mode) = do
        (refStakeOwner, refBeacon) <- asks envAuxiliary
        let beaconSymb = beaconCurrencySymbol refBeacon
            stakeOwnerSymb = beaconCurrencySymbol refStakeOwner
            red' = (par, input, proof, sign verifierPrvKey $ hashRedeemer red)
            bp   = parseBulletproofParams $ sha2_256 $ toBytes par
            v    = fst input
            ins  = map (\(bs, p) -> Input (fromMaybe (throw IncorrectInput) $ toGroupElement bs) p) $ snd input
        unless (verify bulletproofSetup bp v ins proof) $ throwM IncorrectProof
        pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) (beaconSymb, verifierPKH) stakeOwnerSymb red' mode]

instance IsCardanoServerError (TxEndpointsErrorOf EncoinsServer) where
    errStatus _ = toEnum 422
    errMsg = \case
        CorruptedExternalUTXOs -> "The request contained corrupted external UTXO data."
        IncorrectInput         -> "The request contained incorrect public input."
        IncorrectProof         -> "The request contained incorrect proof."

-- instance HasClient EncoinsServer where
--     type ClientInput EncoinsServer = [EncoinsRequestTerm]

--     parseClientInput = some $ mintParser <|> burnParser

--     toServerInput terms = do
--         beaconRef      <- asks envAuxiliary
--         gammas         <- mapM (const randomIO) [1:: Integer ..]
--         randomness     <- randomIO
--         let f = \case
--                 RPMint a -> Just $ toFieldElement $ getLovelace a
--                 _        -> Nothing
--             g = \case
--                 RPBurn (Left s) -> Just s
--                 _                -> Nothing
--             secretsMint  = map (, Mint) $ zipWith Secret gammas $ mapMaybe f terms
--             secretsBurn  = map (, Burn) $ mapMaybe g terms
--             (secrets, ps) = unzip $ secretsBurn ++ secretsMint
--             addr    = stakingValidatorAddress $ encoinsSymbol (beaconCurrencySymbol beaconRef, verifierPKH)
--             par    = parseBulletproofParams $ toBytes addr
--             inputs      = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
--             (v, _, proof) = bulletproof bulletproofSetup par secrets ps randomness
--             signature  = ""
--         pure (addr, (addr, (v, inputs), proof, signature))
