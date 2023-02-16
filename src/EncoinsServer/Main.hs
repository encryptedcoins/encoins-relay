{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module EncoinsServer.Main where

import           CSL                                      (TransactionUnspentOutputs)
import           CSL.Class                                (fromCSL)
import           Cardano.Server.Class                     (Env (..), HasServer (..))
import           Cardano.Server.Client.Class              (HasClient (..))
import           Cardano.Server.Endpoints.Tx.Class        (HasTxEndpoints (..))
import           Cardano.Server.Error                     (IsCardanoServerError (..))
import           Cardano.Server.Input                     (InputContext (..))
import           Cardano.Server.Internal                  (runAppM)
import           Cardano.Server.Main                      (runServer)
import           Cardano.Server.Tx                        (mkTx)
import           Control.Applicative                      (Alternative (..))
import           Control.Exception                        (throw)
import           Control.Monad                            (void)
import           Control.Monad.Reader                     (asks)
import           Data.Default                             (def)
import           Data.Map                                 (fromList)
import           Data.Maybe                               (fromJust, mapMaybe)
import           ENCOINS.BaseTypes                        (MintingPolarity (..))
import           ENCOINS.Bulletproofs                     (Secret (..), bulletproof, fromSecret, parseBulletproofParams)
import           ENCOINS.Core.OffChain                    (beaconMintTx, beaconSendTx, encoinsTx)
import           ENCOINS.Core.OnChain                     (beaconCurrencySymbol, bulletproofSetup, encoinsSymbol,
                                                           stakingValidatorAddress)
import           ENCOINS.Core.V1.OffChain                 (EncoinsRedeemerWithData, postEncoinsPolicyTx, postStakingValidatorTx,
                                                           verifierPKH)
import           ENCOINS.Core.V1.OnChain                  (hashRedeemer)
import           ENCOINS.Crypto.Field                     (toFieldElement)
import           EncoinsServer.Opts                       (EncoinsRequestTerm (..), ServerMode (Run, Setup), burnParser,
                                                           mintParser, runWithOpts)
import           Ledger                                   (TxId (..), TxOutRef (..))
import           Ledger.Ada                               (Ada (..))
import           PlutusAppsExtra.IO.Wallet                (getWalletUtxos)
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Utils.Crypto             (sign)
import           PlutusTx.Extra.ByteString                (ToBuiltinByteString (..))
import           PlutusTx.Prelude                         (toBuiltin)
import           System.Random                            (randomIO)
import           Text.Hex                                 (decodeHex)

runEncoinsServer :: IO ()
runEncoinsServer = do
    mode <- runWithOpts
    case mode of
        Run   -> runServer @EncoinsServer
        Setup -> runAppM (serverSetup @EncoinsServer)

data EncoinsServer

instance HasServer EncoinsServer where

    type AuxiliaryEnvOf EncoinsServer = TxOutRef

    type InputOf EncoinsServer = EncoinsRedeemerWithData

    serverSetup = void $ do
        txOutRef <- asks envAuxiliary
        utxos <- getWalletUtxos
        -- Mint the beacon
        mkTx [] (InputContextServer utxos) [beaconMintTx txOutRef]
        utxos' <- getWalletUtxos
        -- Send it to the staking address
        mkTx [] (InputContextServer utxos') [beaconSendTx txOutRef]
        let encoinsParams = (beaconCurrencySymbol txOutRef, verifierPKH)
            stakingParams = encoinsSymbol encoinsParams
        -- Post the ENCOINS minting policy
        mkTx [] def [postEncoinsPolicyTx encoinsParams 7]
        -- Post the staking validator policy
        mkTx [] def [postStakingValidatorTx stakingParams 7]

    serverIdle = pure ()

    serverTrackedAddresses = do
        ref <- asks envAuxiliary
        let symb = encoinsSymbol (beaconCurrencySymbol ref, verifierPKH)
        return [stakingValidatorAddress symb, alwaysFalseValidatorAddress 7]

instance HasTxEndpoints EncoinsServer where
    type TxApiRequestOf EncoinsServer = (InputOf EncoinsServer, TransactionUnspentOutputs)

    data TxEndpointsErrorOf EncoinsServer = CorruptedExternalUTXOs | IncorrectVerifierSignature
        deriving (Show)

    txEndpointsProcessRequest (redWithData@(addr, _), utxosCSL) = do
        let utxos   = maybe (throw CorruptedExternalUTXOs) fromList $ fromCSL utxosCSL
            context = InputContextClient utxos utxos
                (TxOutRef (TxId "") 1)
                addr
        return (redWithData, context)

    txEndpointsTxBuilders (addr, red@(par, input, proof, _)) = do
        bcs <- asks $ beaconCurrencySymbol . envAuxiliary
        let prvKey = toBuiltin $ fromJust $ decodeHex "1DA4194798C1D3AA8B7E5E39EDA1F130D9123ACCC8CA31A82E033A6D007DA7EC"
            red' = (par, input, proof, sign prvKey $ hashRedeemer red)
        pure [encoinsTx (bcs, verifierPKH) (addr, red')]

instance IsCardanoServerError (TxEndpointsErrorOf EncoinsServer) where
    errStatus _ = toEnum 422
    errMsg = \case
        CorruptedExternalUTXOs -> "The request contained corrupted external UTXO data."
        IncorrectVerifierSignature -> "The request contained incorrect verifier signature."

instance HasClient EncoinsServer where
    type ClientInput EncoinsServer = [EncoinsRequestTerm]

    parseClientInput = some $ mintParser <|> burnParser

    toServerInput terms = do
        beaconRef      <- asks envAuxiliary
        gammas         <- mapM (const randomIO) [1:: Integer ..]
        randomness     <- randomIO
        let f = \case
                RPMint a -> Just $ toFieldElement $ getLovelace a
                _        -> Nothing
            g = \case
                RPBurn (Left s) -> Just s
                _                -> Nothing
            secretsMint  = map (, Mint) $ zipWith Secret gammas $ mapMaybe f terms
            secretsBurn  = map (, Burn) $ mapMaybe g terms
            (secrets, ps) = unzip $ secretsBurn ++ secretsMint
            addr    = stakingValidatorAddress $ encoinsSymbol (beaconCurrencySymbol beaconRef, verifierPKH)
            par    = parseBulletproofParams $ toBytes addr
            inputs      = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
            (v, _, proof) = bulletproof bulletproofSetup par secrets ps randomness
            signature  = ""
        pure (addr, (addr, (v, inputs), proof, signature))