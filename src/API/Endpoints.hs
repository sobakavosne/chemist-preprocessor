{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.Endpoints
  ( api
  , healthHandler
  , server
  ) where

import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Data.Bool                       (bool)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Time                       (getCurrentTime)
import           DataTypes                       (HealthCheck (..))
import           Infrastructure.Config           (loadBoltCfg)
import           Infrastructure.Database         (checkNeo4j, withNeo4j)
import           Network.HTTP.Types.Header       (hContentType)
import qualified Servant                         as S
import           System.Log.FastLogger           (ToLogStr (toLogStr),
                                                  defaultBufSize, flushLogStr,
                                                  pushLogStrLn)
import           System.Log.FastLogger.LoggerSet (LoggerSet, newStdoutLoggerSet)

type API
   = "health" S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] HealthCheck)

api :: S.Proxy API
api = S.Proxy

initLogger :: IO LoggerSet
initLogger = newStdoutLoggerSet defaultBufSize

logInfo :: LoggerSet -> String -> IO ()
logInfo logger msg = do
  timestamp <- getCurrentTime
  (pushLogStrLn logger . toLogStr) (unwords [show timestamp, "-", msg])
  flushLogStr logger

healthHandler ::
     S.Handler (S.Headers '[ S.Header "Content-Type" String] HealthCheck)
healthHandler = do
  logger <- liftIO initLogger
  boltCfg <- liftIO loadBoltCfg
  neo4jStatus <- (liftIO . withNeo4j boltCfg) checkNeo4j
  let neo4jMessage = bool "Neo4j is down" "Neo4j is alive" neo4jStatus
  let health = HealthCheck {status = "Server is alive", neo4j = neo4jMessage}
  (liftIO . logInfo logger . show) health
  if neo4jStatus
    then return $ S.addHeader "application/json" health
    else S.throwError
           S.err500
             { S.errBody = LBS.pack "Neo4j is down"
             , S.errHeaders = [(hContentType, BS.pack "application/json")]
             }

server :: S.Server API
server = healthHandler
