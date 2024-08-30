{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers
  ( LogLevel(..)
  , initLogger
  , unrecord
  , logWith
  , log
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

data LogLevel
  = Error
  | Info
  deriving (Eq)

instance Show LogLevel where
  show Error = "[Error]"
  show Info  = "[Info]"

-- | Unpack the list of `result` records with `key`
unrecord ::
     (Traversable t, Monad m, RecordValue b)
  => t Record
  -> Text
  -> BoltActionT m (t b)
unrecord result key = forM result (`at` key)

initLogger :: IO LoggerSet
initLogger = newStdoutLoggerSet defaultBufSize

logWith :: LogLevel -> LoggerSet -> String -> IO ()
logWith level logger msg = do
  timestamp <- getCurrentTime
  (pushLogStrLn logger . toLogStr . unwords)
    [show timestamp, "-", show level, msg]
  flushLogStr logger
