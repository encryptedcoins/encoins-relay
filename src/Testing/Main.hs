{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Testing.Main (Testing) where
    
import Client.Internal         (HasClient(..))
import Control.Monad           (replicateM)
import Options.Applicative     (argument, metavar, str)
import Plutus.V2.Ledger.Api    (BuiltinByteString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Server.Internal         (HasServer(..))
import Server.Tx               (mkTx)
import System.Random           (randomRIO, randomIO)
import Testing.OffChain        (testCurrencySymbol, testMintTx)

data Testing

instance HasServer Testing where

    type AuxiliaryEnvOf Testing = ()

    loadAuxiliaryEnv _ = pure ()

    type RedeemerOf Testing = [BuiltinByteString]

    getCurrencySymbol = pure testCurrencySymbol

    processTokens bbs = mkTx [testMintTx bbs]

    setupServer _ = pure ()

instance HasClient Testing where

    type RequestPieceOf Testing = BuiltinByteString

    parseRequestPiece = stringToBuiltinByteString <$> argument str (metavar "token name")

    genRequestPiece = fmap stringToBuiltinByteString $ 
        randomRIO (0, 16) >>= (`replicateM` randomIO)

    mkRedeemer = pure . (pure (),)
