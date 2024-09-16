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

log :: (Show a, Show b) => Either a b -> IO ()
log obj = do
  logger <- newStdoutLoggerSet defaultBufSize
  timestamp <- getCurrentTime
  either (logWith logger Error timestamp) (logWith logger Info timestamp) obj
  flushLogStr logger
