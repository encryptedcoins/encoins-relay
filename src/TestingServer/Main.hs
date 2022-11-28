{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module TestingServer.Main (TestingServer) where

import Client.Internal         (HasClient(..))
import Control.Monad.Catch     (Exception, throwM)
import Control.Monad           (replicateM, when, void)
import Data.List               (nub)
import Data.Text               (Text)
import IO.Wallet               (getWalletAddr)
import Options.Applicative     (argument, metavar, str)
import Plutus.V2.Ledger.Api    (BuiltinByteString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Servant                 (NoContent, WithStatus)
import Server.Endpoints.Mint   (HasMintEndpoint(..))
import Server.Internal         (HasServer(..))
import Server.Tx               (mkTx)
import System.Random           (randomRIO, randomIO)
import TestingServer.OffChain  (testCurrencySymbol, testMintTx)
import Utils.Servant           (respondWithStatus)

data TestingServer

instance HasServer TestingServer where

    type AuxiliaryEnvOf TestingServer = ()

    loadAuxiliaryEnv _ = pure ()

    type RedeemerOf TestingServer = [BuiltinByteString]

    getCurrencySymbol = pure testCurrencySymbol

    processTokens bbs = do
        addr <- getWalletAddr
        void $ mkTx [addr] [testMintTx bbs]

    setupServer = pure ()

instance HasMintEndpoint TestingServer where

    type MintApiResultOf TestingServer = '[NoContent, WithStatus 422 Text]

    data (MintErrorOf TestingServer) = HasDuplicates
        deriving (Show, Exception)

    checkForMintErros bbs =
        let hasDuplicates = length bbs /= length (nub bbs)
        in  when hasDuplicates $ throwM HasDuplicates
        
    mintErrorHanlder _ = respondWithStatus @422
        "The request contains duplicate tokens and will not be processed."

instance HasClient TestingServer where

    type RequestPieceOf TestingServer = BuiltinByteString

    parseRequestPiece = stringToBuiltinByteString <$> argument str (metavar "token name")

    genRequestPiece = fmap stringToBuiltinByteString $
        randomRIO (2, 8) >>= (`replicateM` randomIO)

    mkRedeemer = pure . (pure (),)
