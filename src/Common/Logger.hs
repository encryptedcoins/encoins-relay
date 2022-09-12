{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Common.Logger where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Time              as Time

class MonadIO m => HasLogger m where
    
    loggerFilePath :: FilePath

    logMsg :: Text -> m ()
    logMsg msg = liftIO $ logMsgIO msg $ loggerFilePath @m

    logSmth :: Show a => a -> m ()
    logSmth smth = liftIO $ logSmthIO smth $ loggerFilePath @m

logMsgIO :: Text -> FilePath -> IO ()
logMsgIO msg (("logs/" <>) -> fp) = do
        utcTime <- Time.getCurrentTime
        let localTime = Time.addUTCTime (10800 :: Time.NominalDiffTime) utcTime
            asctime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
            msg' = T.pack asctime <> " " <> "\n" <> msg <> "\n\n"
        T.putStrLn msg'
        T.appendFile fp msg'

logSmthIO :: Show a => a -> FilePath -> IO ()
logSmthIO smth = logMsgIO (T.pack $ show smth)

(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> "\n" <> T.pack (show a)

infixr 7 .<