{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Encoins where

import           Cardano.Api.Shelley             (NetworkMagic(..), NetworkId(..))
import           Common.Logger                   (logPretty)
import           Control.Monad                   (forM_, when)
import           Data.Aeson                      (decode)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.Default                    (def)
import           Data.FileEmbed                  (embedFile)
import           Data.Maybe                      (fromJust)
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import           Data.Text.Class                 (FromText(fromText))
import           ENCOINS.Core.BaseTypes          (MintingPolarity(..), toGroupElement)
import           ENCOINS.Core.Bulletproofs.Types (Input(..))
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, beaconMintTx, beaconSendTx)
import           Ledger                          (Params(..))
import           Server.Config                   (Config(..), loadConfig, loadRestoreWallet)
import           Server.Endpoints.Balance        (Balance(..), getBalance)
import           Server.Endpoints.Mint           (processTokens, runQueueM)
import           Server.Internal                 (Env(..))
import           Server.ServerTx                 (mkTxWithConstraints)
import           IO.Wallet                       (HasWallet(..), getWalletAddrBech32, getWalletTxOutRefs, ownAddresses)
import           Utils.Address                   (bech32ToAddress, bech32ToKeyHashes)



instance HasWallet IO where
    getRestoreWallet = loadRestoreWallet

mkRefs :: IO ()
mkRefs = do
    walletAddrBech32 <- getWalletAddrBech32
    let protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
        (walletPKH, walletSKH) = case bech32ToKeyHashes <$> fromText walletAddrBech32 of
            Right (Just res) -> res
            _                -> error "Can't get key hashes from bech32 wallet."
    refs <- getWalletTxOutRefs ledgerParams walletPKH walletSKH 1
    logPretty refs

setup :: IO ()
setup = do
    Config{..} <- loadConfig
    mkTxWithConstraints
        [ beaconMintTx confBeaconTxOutRef
        , beaconSendTx confBeaconTxOutRef
        ]

encoinsMintTest :: IO ()
encoinsMintTest = do
    Config{..} <- loadConfig
    let env = Env undefined confBeaconTxOutRef confWallet
        inputs = [Input (fromJust $ toGroupElement $ fromString $ "aaaa") Mint]
    runQueueM env $ processTokens inputs

balanceTest :: Text -> IO ()
balanceTest addrBech32 = do
    Config{..} <- loadConfig
    let bcs = beaconCurrencySymbol confBeaconTxOutRef
        addr = fromJust $ bech32ToAddress addrBech32
    getBalance bcs addr >>= \(Balance b) -> print b

balanceTestAll :: IO ()
balanceTestAll = do 
    Config{..} <- loadConfig
    let bcs = beaconCurrencySymbol confBeaconTxOutRef
    as <- ownAddresses
    forM_ as $ \a -> do
        let addr = fromJust $ bech32ToAddress a
        Balance res <- getBalance bcs addr
        when (not $ null res) $ do
            print a
            print res