{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Encoins where

import qualified Client.Internal                 as Client
import           Cardano.Api.Shelley             (NetworkMagic(..), NetworkId(..))
import           Utils.Logger                    (logPretty)
import           Control.Monad                   (forM_, unless)
import           Data.Aeson                      (decode)
import           Data.ByteString.Lazy            (fromStrict)
import           Data.Default                    (def)
import           Data.FileEmbed                  (embedFile)
import           Data.Maybe                      (fromJust)
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import           ENCOINS.Core.BaseTypes          (MintingPolarity(..))
import           ENCOINS.Core.Bulletproofs.Types (Input(..))
import           ENCOINS.Core.OffChain           (beaconCurrencySymbol, beaconMintTx, beaconSendTx, encoinsSymbol)
import           Ledger                          (Ada, Params(..))
import           Server.Config                   (Config(..), loadConfig, loadRestoreWallet)
import           Server.Endpoints.Balance        (Balance(..), getBalance)
import           Server.Endpoints.Mint           (processTokens, runQueueM)
import           Server.Internal                 (Env(..))
import           Server.Tx                       (mkTxWithConstraints)
import           IO.Wallet                       (HasWallet(..), getWalletKeyHashes, getWalletTxOutRefs, ownAddresses)
import           Utils.Address                   (bech32ToAddress)

instance HasWallet IO where
    getRestoreWallet = loadRestoreWallet

mkRefs :: IO ()
mkRefs = do
    (walletPKH, walletSKH) <- getWalletKeyHashes
    let protocolParams = fromJust . decode $ fromStrict $(embedFile "testnet/protocol-parameters.json")
        networkId = Testnet $ NetworkMagic 1097911063
        ledgerParams = Params def protocolParams networkId
    refs <- getWalletTxOutRefs ledgerParams walletPKH walletSKH 1
    logPretty refs

setup :: IO ()
setup = do
    Config{..} <- loadConfig
    mkTxWithConstraints
        [ beaconMintTx confBeaconTxOutRef
        , beaconSendTx confBeaconTxOutRef
        ]

encoinsTest :: MintingPolarity  -> Ada -> String -> IO ()
encoinsTest polarity ada name = do
    Config{..} <- loadConfig
    let env = Env undefined confBeaconTxOutRef confWallet
        inputs = [Input (fromString name) polarity]
    red <- Client.runClientM (Client.Env confBeaconTxOutRef confWallet) $ Client.mkRedeemer inputs ada
    runQueueM env $ processTokens red

encoinsTestMint :: String -> IO ()
encoinsTestMint = encoinsTest Mint 5_000_000

encoinsTestBurn :: String -> IO ()
encoinsTestBurn = encoinsTest Burn 5_000_000

balanceTest :: Text -> IO ()
balanceTest addrBech32 = do
    Config{..} <- loadConfig
    let ecs = encoinsSymbol $ beaconCurrencySymbol confBeaconTxOutRef
        addr = fromJust $ bech32ToAddress addrBech32
    getBalance ecs addr >>= \(Balance b) -> print b

balanceTestAll :: IO ()
balanceTestAll = do
    Config{..} <- loadConfig
    let ecs = encoinsSymbol $ beaconCurrencySymbol confBeaconTxOutRef
    as <- ownAddresses
    forM_ as $ \a -> do
        let addr = fromJust $ bech32ToAddress a
        Balance res <- getBalance ecs addr
        unless (null res) $ do
            print a
            print res