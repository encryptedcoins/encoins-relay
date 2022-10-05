{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Common.Logger where

import           Control.Exception      (handle, throw)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Time              as Time
import           GHC.IO.Exception       (IOException(..), IOErrorType(NoSuchThing))
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath.Posix  (takeDirectory)

class MonadIO m => HasLogger m where
    
    loggerFilePath :: FilePath

    logMsg :: Text -> m ()
    logMsg msg = liftIO $ logMsgIO msg $ loggerFilePath @m

    logSmth :: Show a => a -> m ()
    logSmth smth = liftIO $ logSmthIO smth $ loggerFilePath @m

logMsgIO :: Text -> FilePath -> IO ()
logMsgIO msg fileName = handle (handler msg fileName) $ do
    utcTime <- Time.getCurrentTime
    let localTime = Time.addUTCTime (10800 :: Time.NominalDiffTime) utcTime
        asctime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y" localTime
        msg' = T.pack asctime <> " " <> "\n" <> msg <> "\n\n"
    T.putStrLn msg'
    T.appendFile (mkFullPath fileName) msg'

handler :: Text -> FilePath -> IOException -> IO ()
handler msg fileName err 
    | ioe_type err == NoSuchThing = do
        createDirectoryIfMissing True $ takeDirectory $ mkFullPath fileName
        logMsgIO msg fileName
    | otherwise = throw err

mkFullPath :: FilePath -> FilePath
mkFullPath = ("logs/" <>)

logSmthIO :: Show a => a -> FilePath -> IO ()
logSmthIO smth = logMsgIO (T.pack $ show smth)

(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> "\n" <> T.pack (show a)

infixr 7 .<