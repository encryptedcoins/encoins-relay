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

module Encoins.Relay.Poll where

import           Cardano.Api                          (EraInMode (..), FromJSON, ToJSON, writeFileJSON)
import           Cardano.Node.Emulator.TimeSlot       (posixTimeToEnclosingSlot, utcTimeToPOSIXTime)
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Control.Exception                    (SomeException, handle, try)
import           Control.Monad                        (forM, guard, join, void, when)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Default                         (def)
import           Data.Foldable.Extra                  (notNull)
import           Data.Function                        (on)
import           Data.Functor                         ((<&>))
import           Data.List                            (groupBy, partition, sortBy)
import           Data.Maybe                           (catMaybes, listToMaybe, mapMaybe)
import           Data.Time                            (UTCTime, defaultTimeLocale, parseTimeM)
import           Ledger                               (Address (..), AssetClass, CurrencySymbol, Datum (..),
                                                       PaymentPubKeyHash (..), PubKeyHash (..), Slot (..), SomeCardanoApiTx (..),
                                                       TokenName, TxId (..))
import           Ledger.Tx.CardanoAPI                 (getRequiredSigners)
import           Ledger.Value                         (AssetClass (..))
import           Plutus.V1.Ledger.Api                 (BuiltinByteString, Credential (PubKeyCredential), FromData (..),
                                                       StakingCredential (..))
import           PlutusAppsExtra.IO.ChainIndex.Kupo   (getDatumByHashSafe, getTokenBalanceToSlotByPkh)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo   as Kupo
import           PlutusAppsExtra.IO.ChainIndex.Plutus (getTxFromId)
import           PlutusAppsExtra.Utils.Kupo           (KupoResponse (..), SlotWithHeaderHash (..))
import           PlutusTx.Builtins                    (decodeUtf8, fromBuiltin)
import           System.Directory.Extra               (createDirectoryIfMissing, doesFileExist)

poll1 :: IO ()
poll1 = do
    createDirectoryIfMissing True "pollFiles"

    voteStart <- utcToSlot <$> parseTimeM True defaultTimeLocale "%Y-%m-%d"    "2023-06-07"
    voteEnd   <- utcToSlot <$> parseTimeM True defaultTimeLocale "%Y-%m-%d-%H" "2023-06-13-22"
    
    putStrLn "Getting votes..."
    !votesWithDuplicates <- getSmthPartially 
        (\f t -> Kupo.getKupoResponseBetweenSlots f t >>= (fmap catMaybes <$> mapM getVoteFromKupoResponse)) voteStart voteEnd 3600
    let votes = mapMaybe (listToMaybe . reverse) 
            $ groupBy ((==) `on` (\(pkh,_,_, _) -> pkh)) 
            $ sortBy (compare `on` (\(pkh,_,_, _) -> pkh)) votesWithDuplicates
    writeFileJSON "pollFiles/votes" votes
    
    !votesWithWeight <- forM votes $ \(pkh, _, txId, vote) -> do
        let getWeight = getTokenBalanceToSlotByPkh encoinsCS encoinsTokenName voteEnd pkh
        weight <- reloadHandler getWeight
        pure (pkh, weight, txId, fromBuiltin $ decodeUtf8 vote)
    let getVotesNum = sum . map (\(_,v,_, _) -> v)
        (y, n) = partition (\(_, _, _, v) -> v == "Yes") votesWithWeight
        totalV = getVotesNum votesWithWeight
        yV = getVotesNum y
        nV = getVotesNum n
        getPercents x = show @Double ((* 100) (((/) `on` fromIntegral) x totalV)) <> "%"
        msg =  "Yes: " <> getPercents yV <> "\n" <> "No: "  <> getPercents nV
    putStrLn msg
    writeFileJSON "pollFiles/result" msg
    void $ writeFileJSON "pollFiles/resultFull.json" votesWithWeight

getSmthPartially :: forall smth. (Show smth, MkFileName smth)
    => (Maybe Slot -> Maybe Slot -> IO [smth]) -> Slot -> Slot -> Slot -> IO [smth]
getSmthPartially getSmth slotFrom slotTo slotDelta = concat <$> do
        let xs = [slotFrom, slotFrom + slotDelta .. slotTo]
            intervals = zip (init xs) (tail xs)
        forM (zip [1 :: Int ..] intervals) $ \(i, (f, t)) -> getPortion i f t (length intervals)
    where
        getPortion i f t len = reloadHandler $ do
            putStrLn $ show i <> "/" <> show len
            let fName = mkFilePath @smth f t
            !smth <- doesFileExist fName >>= \ex -> if ex
                    then try @SomeException (decodeOrErrorFromFile fName) >>= either (const $ (getSmth `on` Just) f t) pure
                    else (getSmth `on` Just) f t
            writeFileJSON fName smth
            when (notNull smth) $ print smth
            pure smth

class (ToJSON a, FromJSON a) => MkFileName a where
    fileName :: FilePath

instance MkFileName (PubKeyHash, Slot, TxId, BuiltinByteString) where fileName = "votes"

mkFilePath :: forall a. MkFileName a =>  Slot ->  Slot -> FilePath
mkFilePath from to = "pollFiles/" <> fileName @a <> "_" <> show (getSlot from) <> "_"  <> show (getSlot to) <> ".json"

poll1Rules :: Datum -> Maybe (BuiltinByteString, BuiltinByteString)
poll1Rules (Datum dat) =  case fromBuiltinData dat of
    Just ["ENCOINS", "Poll #1", bs2, bs3]
        -> if bs3 == "Yes" || bs3 == "No"
           then Just (bs2, bs3)
           else Nothing
    _ -> Nothing

getVoteFromKupoResponse :: KupoResponse -> IO (Maybe (PubKeyHash, Slot, TxId, BuiltinByteString))
getVoteFromKupoResponse KupoResponse{..} = runMaybeT $ do
        dat <- MaybeT $ fmap join $ sequence $ getDatumByHashSafe <$> krDatumHash
        (pkhBbs, vote) <- hoistMaybeT $ poll1Rules dat
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
reloadHandler ma = handle (\(e :: SomeException) -> putStrLn (show e <> "\t(Handled)") >> reloadHandler ma) ma

encoinsTokenName :: TokenName
encoinsTokenName = "ENCS"

encoinsCS :: CurrencySymbol
encoinsCS = "9abf0afd2f236a19f2842d502d0450cbcd9c79f123a9708f96fd9b96"

encoinsAssetClass :: AssetClass
encoinsAssetClass = AssetClass (encoinsCS, encoinsTokenName)

utcToSlot :: UTCTime -> Slot
utcToSlot = (+ 4492827) . posixTimeToEnclosingSlot def . utcTimeToPOSIXTime