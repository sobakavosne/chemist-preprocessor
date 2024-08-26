{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers
  ( unrecord
  , initLogger
  , logInfo
  ) where

import           Control.Monad                   (forM)
import           Data.Text                       (Text)
import           Data.Time                       (getCurrentTime)
import           Database.Bolt                   (BoltActionT, Record,
                                                  RecordValue, at)
import           System.Log.FastLogger           (LoggerSet,
                                                  ToLogStr (toLogStr),
                                                  defaultBufSize, flushLogStr,
                                                  newStdoutLoggerSet)
import           System.Log.FastLogger.LoggerSet (pushLogStrLn)

-- | Unpack the list of `result` records with `key`
unrecord ::
     (Traversable t, Monad m, RecordValue b)
  => t Record
  -> Text
  -> BoltActionT m (t b)
unrecord result key = forM result (`at` key)

initLogger :: IO LoggerSet
initLogger = newStdoutLoggerSet defaultBufSize

logInfo :: LoggerSet -> String -> IO ()
logInfo logger msg = do
  timestamp <- getCurrentTime
  pushLogStrLn logger . toLogStr $ unwords [show timestamp, "-", msg]
  flushLogStr logger
