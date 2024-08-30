{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module API.Endpoints
  ( api
  , server
  ) where

import           API.Type                   (HealthCheck (..))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Domain.Service             (getReaction)
import           Helpers                    (LogLevel (Info), initLogger,
                                             logWith)
import           Infrastructure.Config      (loadBoltCfg)
import           Infrastructure.Database    (checkNeo4j, withNeo4j)
import           Models                     (ReactionDetails)
import           Network.HTTP.Types.Header  (hContentType)
import           Prelude                    hiding (id)
import qualified Servant                    as S

type API =
  "health" S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] HealthCheck) S.:<|> 
  "reaction" S.:> S.Capture "id" Int S.:> S.Get '[ S.JSON] (S.Headers '[ S.Header "Content-Type" String] ReactionDetails)

api :: S.Proxy API
api = S.Proxy

contentTypeJson :: String
contentTypeJson = "application/json"

healthHandler ::
     S.Handler (S.Headers '[ S.Header "Content-Type" String] HealthCheck)
healthHandler = do
  logger <- liftIO initLogger
  result <- liftIO . withNeo4j checkNeo4j =<< liftIO loadBoltCfg
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

reactionHandler ::
     Int
  -> S.Handler (S.Headers '[ S.Header "Content-Type" String] ReactionDetails)
reactionHandler id = do
  logger <- liftIO initLogger
  result <- (liftIO . getReaction) id
  (liftIO . logWith Info logger . show) result
  (return . S.addHeader contentTypeJson) result

server :: S.Server API
server = healthHandler S.:<|> reactionHandler
