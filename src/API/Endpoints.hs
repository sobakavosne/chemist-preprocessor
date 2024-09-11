{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.Endpoints
  ( api
  , server
  ) where

import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Data.Aeson                      (FromJSON, ToJSON)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Time                       (getCurrentTime)
import           Domain.Service                  (deleteReaction, getMechanism,
                                                  getPath, getReaction,
                                                  postReaction)
import           GHC.Generics                    (Generic)
import           Infrastructure.Database         (checkNeo4j, withNeo4j)
import           Models                          (MechanismDetails, PathMask,
                                                  ProcessDetails (..), Reaction,
                                                  ReactionDetails)
import           Network.HTTP.Types.Header       (hContentType)
import           Prelude                         hiding (id)
import qualified Servant                         as S
import           System.Log.FastLogger           (LoggerSet,
                                                  ToLogStr (toLogStr),
                                                  defaultBufSize, flushLogStr,
                                                  newStdoutLoggerSet)
import           System.Log.FastLogger.LoggerSet (pushLogStrLn)

import           Control.Exception               (Exception, throw)
import           Data.Bool                       (bool)
import           Data.Text                       (Text)

newtype MismatchError =
  MismatchError Text
  deriving (Show)

instance Exception MismatchError

data HealthCheck =
  HealthCheck
    { status       :: String
    , neo4jMessage :: String
    }
  deriving (Show, Generic)

instance ToJSON HealthCheck

instance FromJSON HealthCheck

data LogLevel
  = Error
  | Info
  deriving (Eq)

instance Show LogLevel where
  show Error = "[Error]"
  show Info  = "[Info]"

initLogger :: IO LoggerSet
initLogger = newStdoutLoggerSet defaultBufSize

logWith :: LogLevel -> LoggerSet -> String -> IO ()
logWith level logger msg = do
  timestamp <- getCurrentTime
  (pushLogStrLn logger . toLogStr . unwords)
    [show timestamp, "-", show level, msg]
  flushLogStr logger

type API =
  "health"    S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] HealthCheck) S.:<|>
  "reaction"  S.:> S.Capture "id" Int S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] ReactionDetails) S.:<|>
  "reaction"  S.:> S.ReqBody '[ S.JSON] ReactionDetails S.:> S.Post '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] Reaction) S.:<|>
  "reaction"  S.:> S.Capture "id" Int S.:> S.Delete '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] Int) S.:<|>
  "path"      S.:> S.Capture "start" Int S.:> S.Capture "end" Int S.:> S.Get '[S.JSON] (S.Headers '[S.Header "Content-Type" String] PathMask) S.:<|>
  "mechanism" S.:> S.Capture "id" Int S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] MechanismDetails) S.:<|>
  "process"   S.:> S.Capture "reactionId" Int S.:> S.Capture "mechanismId" Int S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] ProcessDetails)

api :: S.Proxy API
api = S.Proxy

contentTypeJson :: String
contentTypeJson = "application/json"

healthHandler ::
     S.Handler (S.Headers '[ S.Header "Content-Type" String] HealthCheck)
healthHandler = do
  logger <- liftIO initLogger
  result <- (liftIO . withNeo4j) checkNeo4j
  let neo4jMessage =
        if result
          then "Neo4j is alive"
          else "Neo4j is down"
  let health = HealthCheck {status = "Server is alive", neo4jMessage}
  (liftIO . logWith Info logger . show) health
  if result
    then return $ S.addHeader "application/json" health
    else S.throwError
           S.err500
             { S.errBody = (LBS.pack . show) health
             , S.errHeaders = [(hContentType, BS.pack contentTypeJson)]
             }

getReactionHandler ::
     Int
  -> S.Handler (S.Headers '[ S.Header "Content-Type" String] ReactionDetails)
getReactionHandler id = do
  logger <- liftIO initLogger
  result <- (liftIO . getReaction) id
  (liftIO . logWith Info logger . show) result
  (return . S.addHeader contentTypeJson) result

postReactionHandler ::
     ReactionDetails
  -> S.Handler (S.Headers '[ S.Header "Content-Type" String] Reaction)
postReactionHandler details = do
  logger <- liftIO initLogger
  result <- liftIO $ postReaction details
  (liftIO . logWith Info logger . show) result
  return $ S.addHeader contentTypeJson result

deleteReactionHandler ::
     Int -> S.Handler (S.Headers '[ S.Header "Content-Type" String] Int)
deleteReactionHandler id = do
  logger <- liftIO initLogger
  result <- liftIO $ deleteReaction id
  (liftIO . logWith Info logger)
    ("Reaction with ID " <> show result <> " deleted.")
  (return . S.addHeader contentTypeJson) result

getPathHandler ::
     Int -- ^ Start molecule ID
  -> Int -- ^ End molecule ID
  -> S.Handler (S.Headers '[ S.Header "Content-Type" String] PathMask)
getPathHandler start end = do
  logger <- liftIO initLogger
  result <- liftIO $ getPath start end
  (liftIO . logWith Info logger . show) result
  (return . S.addHeader contentTypeJson) result

getMechanismHandler ::
     Int
  -> S.Handler (S.Headers '[ S.Header "Content-Type" String] MechanismDetails)
getMechanismHandler id = do
  logger <- liftIO initLogger
  result <- (fmap fst . liftIO . getMechanism) id
  (liftIO . logWith Info logger . show) result
  (return . S.addHeader contentTypeJson) result

getProcessDetailsHandler ::
     Int -- ^ Reaction ID
  -> Int -- ^ Mechanism ID
  -> S.Handler (S.Headers '[ S.Header "Content-Type" String] ProcessDetails)
getProcessDetailsHandler reactionId mechanismId = do
  logger <- liftIO initLogger
  reactionDetails <- liftIO $ getReaction reactionId
  (mechanismDetails, reactionId') <- liftIO $ getMechanism mechanismId
  bool
    (throw
       (MismatchError
          "The requested mechanism does not match the requested reaction"))
    (pure ())
    (reactionId == reactionId')
  let result = ProcessDetails {reactionDetails, mechanismDetails}
  (liftIO . logWith Info logger . show) result
  return $ S.addHeader contentTypeJson result

server :: S.Server API
server =
  healthHandler S.:<|> getReactionHandler S.:<|> postReactionHandler S.:<|>
  deleteReactionHandler S.:<|>
  getPathHandler S.:<|>
  getMechanismHandler S.:<|>
  getProcessDetailsHandler
