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

import           Cardano.Api                          (EraInMode (..), FromJSON, ToJSON, writeFileJSON)
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Control.Exception                    (SomeException, handle, try, AsyncException (UserInterrupt), Exception (fromException))
import           Control.Lens                         ((^.))
import           Control.Lens.Tuple                   (Field1(_1), Field2(_2), Field4(_4))
import           Control.Monad                        (forM, join, void)
import           Control.Monad.Catch                  (MonadThrow(..))
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Aeson                           (eitherDecodeFileStrict)
import           Data.Char                            (isNumber)
import           Data.Function                        (on)
import           Data.Functor                         ((<&>))
import           Data.List                            (groupBy, partition, sortBy)
import           Data.Maybe                           (catMaybes, listToMaybe, mapMaybe)
import           Encoins.Relay.Poll.Config            (PollConfig (..))
import           Ledger                               (Address (..), Datum (..), PaymentPubKeyHash (..),
                                                       PubKeyHash (..), Slot (..), SomeCardanoApiTx (..), TxId (..))
import           Ledger.Tx.CardanoAPI                 (getRequiredSigners)
import           Plutus.V1.Ledger.Api                 (BuiltinByteString, Credential (PubKeyCredential), FromData (..),
                                                       StakingCredential (..))
import           PlutusAppsExtra.IO.ChainIndex.Kupo   (getDatumByHashSafe, getTokenBalanceToSlotByPkh)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo   as Kupo
import           PlutusAppsExtra.IO.ChainIndex.Plutus (getTxFromId)
import           PlutusAppsExtra.Utils.Kupo           (KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusTx.Builtins                    (decodeUtf8, fromBuiltin)
import           System.Directory.Extra               (createDirectoryIfMissing, doesFileExist)
import           System.Environment                   (getArgs)

doPoll :: IO ()
doPoll = getArgs <&> map (filter isNumber) >>= \case
    ["1"] -> poll 1
    _     -> error "unknown poll"

poll :: Int -> IO ()
poll pollNo = do
    PollConfig{..} <-  fmap (either error id) . eitherDecodeFileStrict $ "poll" <> show pollNo <> "config.json"
    let folderName = "poll" <> show pollNo <> "files"
    createDirectoryIfMissing True folderName

    putStrLn "Getting votes..."
    let getVotes f t = Kupo.getKupoResponseBetweenSlots f t >>= (fmap catMaybes <$> mapM getVoteFromKupoResponse)
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

getVoteFromKupoResponse :: KupoResponse -> IO (Maybe (PubKeyHash, Slot, TxId, BuiltinByteString))
getVoteFromKupoResponse KupoResponse{..} = runMaybeT $ do
        dat <- MaybeT $ fmap join $ sequence $ getDatumByHashSafe <$> krDatumHash
        (pkhBbs, vote) <- hoistMaybeT $ extractVoteFromDatum dat
        pkh <-  hoistMaybeT $ getStakeKey krAddress
        -- MaybeT (Just <$> signedBySameKey krTransactionId pkhBbs) >>= guard
        pureMaybeT (pkh, swhhSlot krCreatedAt, krTransactionId,  vote)
    where
        hoistMaybeT = MaybeT . pure
        pureMaybeT = hoistMaybeT . pure
        signedBySameKey :: TxId -> BuiltinByteString -> IO Bool
        signedBySameKey txId pkh = getTxFromId txId <&> \case
                Just (SomeTx tx BabbageEraInCardanoMode) -> PaymentPubKeyHash (PubKeyHash pkh) `elem` getRequiredSigners tx
                _ -> False
        getStakeKey :: Address -> Maybe PubKeyHash
        getStakeKey = \case
            (Address _ (Just (StakingHash (PubKeyCredential pkh)))) -> Just pkh
            _ -> Nothing
        extractVoteFromDatum (Datum dat) = case fromBuiltinData dat of
            Just ["ENCOINS", "Poll #1", bs2, bs3] -> if bs3 == "Yes" || bs3 == "No" then Just (bs2, bs3) else Nothing
            _ -> Nothing

reloadHandler :: IO a -> IO a
reloadHandler ma = (`handle` ma) $ \e -> case fromException e of
    Just UserInterrupt -> throwM UserInterrupt
    _ -> putStrLn (show e <> "\t(Handled)") >> reloadHandler ma
