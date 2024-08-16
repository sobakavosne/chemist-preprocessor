{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Helpers
  ( exactFields
  , fromDouble
  , unrecord
  , initLogger
  , logInfo
  ) where

import           Control.Monad                   (forM)
import           Control.Monad.Except            (MonadError)
import           Data.Map                        ((!))
import           Data.Text                       (Text, pack)
import           Data.Time                       (getCurrentTime)
import           Database.Bolt                   (BoltActionT, Record,
                                                  RecordValue, Structure (..),
                                                  UnpackError, Value (..), at,
                                                  exact)
import           System.Log.FastLogger           (LoggerSet,
                                                  ToLogStr (toLogStr),
                                                  defaultBufSize, flushLogStr,
                                                  newStdoutLoggerSet)
import           System.Log.FastLogger.LoggerSet (pushLogStrLn)

exactFields ::
     (MonadError UnpackError m, RecordValue a) => Text -> Structure -> m a
exactFields key s = exact . (\(M m) -> m ! key) =<< (exact . last . fields) s

fromDouble :: Double -> Float
fromDouble = realToFrac :: Double -> Float

unrecord ::
     (Traversable t, Monad m, RecordValue b)
  => t Record
  -> String
  -> BoltActionT m (t b)
unrecord result key = forM result (`at` pack key)

initLogger :: IO LoggerSet
initLogger = newStdoutLoggerSet defaultBufSize

logInfo :: LoggerSet -> String -> IO ()
logInfo logger msg = do
  timestamp <- getCurrentTime
  pushLogStrLn logger . toLogStr $ unwords [show timestamp, "-", msg]
  flushLogStr logger
