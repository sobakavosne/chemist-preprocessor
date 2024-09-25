-- | This module provides a simple logging functionality for recording
--   messages with different log levels (Error and Info). It uses
--   the FastLogger library to log messages with timestamps to the
--   standard output. The logging system captures `UTCTime` to ensure
--   accurate time-stamped logs. The `log` function can handle both
--   `Left` and `Right` values, logging them accordingly based on their 
--   presence in the context of errors or informational messages.
--
module API.Logger
  ( log
  ) where

import           Data.Time             (UTCTime, getCurrentTime)
import           Prelude               hiding (log)
import           System.Log.FastLogger (LoggerSet, ToLogStr (toLogStr),
                                        defaultBufSize, flushLogStr,
                                        newStdoutLoggerSet, pushLogStrLn)

data LogLevel
  = Error
  | Info
  deriving (Eq)

instance Show LogLevel where
  show Error = "[Error]"
  show Info  = "[Info]"

logWith :: Show a => LoggerSet -> LogLevel -> UTCTime -> a -> IO ()
logWith logger level timestamp x =
  (pushLogStrLn logger . toLogStr . unwords)
    [show timestamp, "-", show level, show x]

-- | Logs a message based on the value of an 'Either' type.
--
--   The 'log' function accepts an 'Either a b' value where:
--
-- * If it is 'Left', it logs an error message with the 'Error'
--   log level.
-- * If it is 'Right', it logs an informational message with the
--   'Info' log level.
--
-- Each log entry is timestamped using the current UTC time, and
-- messages are printed to standard output. The logger is flushed
-- after logging to ensure all messages are displayed promptly.
log :: (Show a, Show b) => Either a b -> IO ()
log obj = do
  logger <- newStdoutLoggerSet defaultBufSize
  timestamp <- getCurrentTime
  either (logWith logger Error timestamp) (logWith logger Info timestamp) obj
  flushLogStr logger
