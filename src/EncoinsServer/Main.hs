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

module EncoinsServer.Main where

import           Control.Exception                        (throw, Exception)
import           Control.Monad                            (void)
import           Data.Aeson                               (decode)
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.Default                             (def)
import           Data.FileEmbed                           (embedFile)
import           Data.Map                                 (fromList)
import           Data.Maybe                               (fromJust)
import           Ledger                                   (TxOutRef (..), TxId (..), Address)
import           PlutusTx.Prelude                         (toBuiltin, BuiltinByteString)
import           Text.Hex                                 (decodeHex, Text)

import           Cardano.Server.Config                    (decodeOrErrorFromFile)
import           Cardano.Server.Error                     (IsCardanoServerError (errMsg, errStatus), toEnvelope)
import           Cardano.Server.Input                     (InputContext (InputContextClient, InputContextServer))
import           Cardano.Server.Internal                  (AuxillaryEnvOf, InputOf, InputWithContext,
                                                           ServerHandle (..), ServerM, getAuxillaryEnv, loadEnv,
                                                           runServerM)
import           Cardano.Server.Main                      (ServerApi, runServer)
import           Cardano.Server.Tx                        (mkTx)

import           CSL                                      (TransactionUnspentOutputs)
import           CSL.Class                                (fromCSL)

import           ENCOINS.Core.OffChain                    (beaconMintTx, beaconSendTx, encoinsTx)
import           ENCOINS.Core.V1.OnChain                  (hashRedeemer)
import           ENCOINS.Core.V1.OffChain                 (EncoinsRedeemerWithData, postEncoinsPolicyTx, postStakingValidatorTx)
import           ENCOINS.Core.OnChain                     (beaconCurrencySymbol, encoinsSymbol, stakingValidatorAddress)
import           EncoinsServer.Opts                       (ServerMode(..), runWithOpts)

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

runEncoinsServer :: IO ()
runEncoinsServer = do
    aEnv <- loadAuxillaryEnv
    mode <- runWithOpts
    case mode of
        Run   -> runServer $ mkServerHandle aEnv
        Setup -> loadEnv (mkServerHandle aEnv) >>= (`runServerM` serverSetup)

mkServerHandle :: AuxillaryEnvOf EncoinsApi -> ServerHandle EncoinsApi
mkServerHandle aEnv = ServerHandle
    Kupo
    aEnv
    getTrackedAddresses
    txBuilders
    (pure ())
    processRequest
    (const $ toEnvelope $ pure "Not implemented yet.")

loadAuxillaryEnv :: IO (AuxillaryEnvOf EncoinsApi)
loadAuxillaryEnv = decodeOrErrorFromFile "txOutRef.json"

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
    let utxos   = maybe (throw CorruptedExternalUTXOs) fromList $ fromCSL utxosCSL
        context = InputContextClient utxos utxos
            (TxOutRef (TxId "") 1)
            addr
    return (redWithData, context)

txBuilders :: InputOf EncoinsApi -> ServerM EncoinsApi [TransactionBuilder ()]
txBuilders (addr, red@(par, input, proof, _)) = do
    bcs <- beaconCurrencySymbol <$> getAuxillaryEnv
    let red' = (par, input, proof, sign verifierPrvKey $ hashRedeemer red)
    pure [encoinsTx (relayWalletAddress, treasuryWalletAddress) (bcs, verifierPKH) (addr, red')]

-- instance HasServer EncoinsServer where

    -- type AuxiliaryEnvOf EncoinsServer = TxOutRef

    -- type InputOf EncoinsServer = EncoinsRedeemerWithData

    -- serverSetup = 

    -- serverIdle = pure ()

    -- serverTrackedAddresses = 

    -- defaultChainIndex = Kupo

-- instance HasTxEndpoints EncoinsServer where
--     type TxApiRequestOf EncoinsServer = (InputOf EncoinsServer, TransactionUnspentOutputs)

    

    -- txEndpointsProcessRequest 

    -- txEndpointsTxBuilders 

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