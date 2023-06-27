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
import           Control.Monad                        (forM, guard, join, void, when)
import           Control.Monad.Catch                  (MonadThrow(..))
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Aeson                           (eitherDecodeFileStrict)
import           Data.Char                            (isNumber)
import           Data.Foldable.Extra                  (notNull)
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
    ["1"] -> poll 1 poll1Rules
    _     -> error "unknown poll"

poll :: Int -> PollRules -> IO ()
poll pollNo rules = do
    PollConfig{..} <-  fmap (either error id) . eitherDecodeFileStrict $ "pollConfig" <> show pollNo <> ".json"
    let folderName = "pollFiles" <> show pollNo
    createDirectoryIfMissing True folderName

    putStrLn "Getting votes..."
    let getVotes f t = Kupo.getKupoResponseBetweenSlots f t >>= (fmap catMaybes <$> mapM (getVoteFromKupoResponse rules))
    !votesWithDuplicates <- getSmthPartially folderName getVotes pcStart pcFinish 3600
    let votes = mapMaybe (listToMaybe . reverse)
            $ groupBy ((==) `on` (\(pkh,_,_, _) -> pkh))
            $ sortBy (compare `on` (\(pkh,_,_, _) -> pkh)) votesWithDuplicates
    writeFileJSON (folderName <> "/votes") votes

    !votesWithWeight <- forM votes $ \(pkh, _, txId, vote) -> do
        let getWeight = getTokenBalanceToSlotByPkh pcCS pcTokenName pcFinish pkh
        weight <- reloadHandler getWeight
        pure (pkh, weight, txId, fromBuiltin $ decodeUtf8 vote)
    let getVotesNum = sum . map (\(_, v, _, _) -> v)
        (y, n) = partition (\(_, _, _, v) -> v == "Yes") votesWithWeight
        totalV = getVotesNum votesWithWeight
        yV = getVotesNum y
        nV = getVotesNum n
        getPercents x = show @Double ((* 100) (((/) `on` fromIntegral) x totalV)) <> "%"
        msg =  "Yes: " <> getPercents yV <> "\n" <> "No: "  <> getPercents nV
    putStrLn msg
    writeFileJSON (folderName <> "/result") msg
    void $ writeFileJSON (folderName <> "/resultFull.json") votesWithWeight

getSmthPartially :: forall smth. (Show smth, FromJSON smth, ToJSON smth)
    => String -> (Maybe Slot -> Maybe Slot -> IO [smth]) -> Slot -> Slot -> Slot -> IO [smth]
getSmthPartially folderName getSmth slotFrom slotTo slotDelta = concat <$> do
        let intervals = divideTimeIntoIntervals slotFrom slotTo slotDelta
        forM (zip [1 :: Int ..] intervals) $ \(i, (f, t)) -> getPortion i f t (length intervals)
    where
        getPortion i f t len = reloadHandler $ do
            putStrLn $ show i <> "/" <> show len
            let fName = folderName <> "/votes" <> "_" <> show (getSlot slotFrom) <> "_"  <> show (getSlot slotTo) <> ".json"
            !smth <- doesFileExist fName >>= \ex -> if ex
                    then try @SomeException (decodeOrErrorFromFile fName) >>= either (const $ (getSmth `on` Just) f t) pure
                    else (getSmth `on` Just) f t
            writeFileJSON fName smth
            when (notNull smth) $ print smth
            pure smth

divideTimeIntoIntervals :: Slot -> Slot -> Slot -> [(Slot, Slot)]
divideTimeIntoIntervals from to delta = do
    let xs = [from, from + delta .. to]
    zip (init xs) (subtract 1 <$> tail xs) <> [(last $ tail xs, to)]

type PollRules = Datum -> Maybe (BuiltinByteString, BuiltinByteString)

poll1Rules :: PollRules
poll1Rules (Datum dat) =  case fromBuiltinData dat of
    Just ["ENCOINS", "Poll #1", bs2, bs3]
        -> if bs3 == "Yes" || bs3 == "No"
           then Just (bs2, bs3)
           else Nothing
    _ -> Nothing

getVoteFromKupoResponse :: PollRules -> KupoResponse -> IO (Maybe (PubKeyHash, Slot, TxId, BuiltinByteString))
getVoteFromKupoResponse rules KupoResponse{..} = runMaybeT $ do
        dat <- MaybeT $ fmap join $ sequence $ getDatumByHashSafe <$> krDatumHash
        (pkhBbs, vote) <- hoistMaybeT $ rules dat
        pkh <-  hoistMaybeT $ getStakeKey krAddress
        MaybeT (Just <$> signedBySameKey krTransactionId pkhBbs) >>= guard
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

reloadHandler :: IO a -> IO a
reloadHandler ma = (`handle` ma) $ \e -> case fromException e of
    Just UserInterrupt -> throwM UserInterrupt
    _ -> putStrLn (show e <> "\t(Handled)") >> reloadHandler ma
