{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.Endpoints
  ( api
  , healthHandler
  , server
  ) where

import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Bool               (bool)
import           DataTypes               (HealthCheck (..))
import           Infrastructure.Config   (loadBoltCfg)
import           Infrastructure.Database (checkNeo4j, withNeo4j)
import qualified Servant                 as S

type API = "health" S.:> S.Get '[ S.JSON] HealthCheck

api :: S.Proxy API
api = S.Proxy

healthHandler :: S.Handler HealthCheck
healthHandler = do
  boltCfg <- liftIO loadBoltCfg
  neo4jStatus <- liftIO $ withNeo4j boltCfg checkNeo4j
  let neo4jMessage = bool "Neo4j is down" "Neo4j is alive" neo4jStatus
  return HealthCheck {status = "Server is alive", neo4j = neo4jMessage}

server :: S.Server API
server = healthHandler
