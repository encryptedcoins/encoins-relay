{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>           #-}

module Encoins.Relay.Poll where

import           Cardano.Api                        (FromJSON, ToJSON, writeFileJSON)
import           Cardano.Server.Config              (decodeOrErrorFromFile)
import           Control.Exception                  (AsyncException (UserInterrupt), Exception (fromException), SomeException,
                                                     handle, try)
import           Control.Lens                       ((^.))
import           Control.Lens.Tuple                 (Field1 (_1), Field2 (_2), Field4 (_4))
import           Control.Monad                      (forM, guard, join, void)
import           Control.Monad.Catch                (MonadThrow (..))
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import           Data.Aeson                         (eitherDecodeFileStrict)
import           Data.Function                      (on)
import           Data.List                          (groupBy, partition, sortBy)
import           Data.Maybe                         (catMaybes, listToMaybe, mapMaybe)
import           Encoins.Relay.Poll.Config          (PollConfig (..))
import           Ledger                             (Datum (..), PubKeyHash (..), Slot (..), TxId (..))
import           Plutus.V1.Ledger.Api               (BuiltinByteString, FromData (..))
import           PlutusAppsExtra.IO.ChainIndex.Kupo (getDatumByHashSafe, getTokenBalanceToSlotByPkh)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo as Kupo
import           PlutusAppsExtra.Utils.Address      (getStakeKey)
import           PlutusAppsExtra.Utils.Kupo         (KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusAppsExtra.Utils.Tx           (txIsSignedByKey)
import           PlutusTx.Builtins                  (decodeUtf8, fromBuiltin)
import           PlutusTx.Builtins.Class            (stringToBuiltinByteString)
import           System.Directory.Extra             (createDirectoryIfMissing, doesFileExist)
import           System.Environment                 (getArgs)
import           Text.Read                          (readEither)

doPoll :: IO ()
doPoll = getArgs >>= \case
    pollNo:_ -> either (error "Incorrect poll number.") poll $ readEither pollNo
    _        -> error "No poll number is given."

poll :: Integer -> IO ()
poll pollNo = do
    PollConfig{..} <-  fmap (either error id) . eitherDecodeFileStrict $ "poll" <> show pollNo <> "config.json"
    let folderName = "poll" <> show pollNo <> "files"
    createDirectoryIfMissing True folderName

    putStrLn "Getting votes..."
    let getVotes f t = Kupo.getKupoResponseBetweenSlots f t >>= (fmap catMaybes <$> mapM (getVoteFromKupoResponse pollNo))
    !votesWithDuplicates <- getSmthPartially folderName getVotes pcStart pcFinish 3600
    let votes = mapMaybe (listToMaybe . reverse)
            $ groupBy ((==) `on` (^. _1))
            $ sortBy (compare `on` (^. _1)) votesWithDuplicates
    writeFileJSON (folderName <> "/votes") votes

    !votesWithWeight <- forM votes $ \(pkh, _, txId, vote) -> do
        let getWeight = getTokenBalanceToSlotByPkh pcCS pcTokenName pcFinish pkh
        weight <- reloadHandler getWeight
        pure (pkh, weight, txId, fromBuiltin $ decodeUtf8 vote)
    let getVotesNum = sum . map (^. _2)
        (y, n) = partition ((== "Yes") . (^. _4)) votesWithWeight
        totalV = getVotesNum votesWithWeight
        yV = getVotesNum y
        nV = getVotesNum n
        getPercents x = show @Double ((* 100) (((/) `on` fromIntegral) x totalV)) <> "%"
        msg =  "Yes: " <> getPercents yV <> "\n" <> "No: "  <> getPercents nV
    putStrLn msg
    writeFileJSON (folderName <> "/result") msg
    void $ writeFileJSON (folderName <> "/resultFull.json") $ map (\(pkh, w, tx, v) -> (pkh, tx, v, w)) votesWithWeight

getSmthPartially :: forall smth. (Show smth, FromJSON smth, ToJSON smth)
    => String -> (Maybe Slot -> Maybe Slot -> IO [smth]) -> Slot -> Slot -> Slot -> IO [smth]
getSmthPartially folderName getSmth slotFrom slotTo slotDelta = concat <$> do
        let intervals = divideTimeIntoIntervals slotFrom slotTo slotDelta
        forM (zip [1 :: Int ..] intervals) $ \(i, (f, t)) -> getPortion i f t (length intervals)
    where
        getPortion i f t len = reloadHandler $ do
            putStrLn $ show i <> "/" <> show len
            let fName = folderName <> "/votes" <> "_" <> show (getSlot f) <> "_"  <> show (getSlot t) <> ".json"
            !smth <- doesFileExist fName >>= \ex -> if ex
                    then try @SomeException (decodeOrErrorFromFile fName) >>= either (const $ (getSmth `on` Just) f t) pure
                    else (getSmth `on` Just) f t
            writeFileJSON fName smth
            mapM_ print smth
            pure smth

divideTimeIntoIntervals :: Slot -> Slot -> Slot -> [(Slot, Slot)]
divideTimeIntoIntervals from to delta = do
    let xs = [from, from + delta .. to]
    zip (init xs) (subtract 1 <$> tail xs) <> [(last xs, to)]

getVoteFromKupoResponse :: Integer -> KupoResponse -> IO (Maybe (PubKeyHash, Slot, TxId, BuiltinByteString))
getVoteFromKupoResponse pollNo KupoResponse{..} = runMaybeT $ do
        dat <- MaybeT $ fmap join $ sequence $ getDatumByHashSafe <$> krDatumHash
        (pkhBbs, vote) <- hoistMaybeT $ extractVoteFromDatum dat
        pkh <-  hoistMaybeT $ getStakeKey krAddress
        -- MaybeT (Just <$> txIsSignedByKey krTransactionId pkhBbs) >>= guard
        pureMaybeT (pkh, swhhSlot krCreatedAt, krTransactionId,  vote)
    where
        hoistMaybeT = MaybeT . pure
        pureMaybeT = hoistMaybeT . pure
        extractVoteFromDatum (Datum dat) = case fromBuiltinData dat of
            Just ["ENCOINS", bs1, bs2, bs3] -> 
                if bs3 == "Yes" || bs3 == "No" && bs1 == "Poll #" <> stringToBuiltinByteString (show pollNo)
                then Just (bs2, bs3) 
                else Nothing
            _ -> Nothing

reloadHandler :: IO a -> IO a
reloadHandler ma = (`handle` ma) $ \e -> case fromException e of
    Just UserInterrupt -> throwM UserInterrupt
    _ -> putStrLn (show e <> "\t(Handled)") >> reloadHandler ma