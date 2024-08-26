{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

module API.Type
  ( HealthCheck(..)
  , ServerConfig(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Default (Default (def))
import           GHC.Generics (Generic)

data HealthCheck =
  HealthCheck
    { status :: String
    , neo4jMessage  :: String
    }
  deriving (Show, Generic)

instance ToJSON HealthCheck

instance FromJSON HealthCheck

data ServerConfig =
  ServerConfig
    { port :: Int
    , host :: String
    }
  deriving (Generic)

instance Default ServerConfig where
  def :: ServerConfig
  def = ServerConfig {port = 8080, host = "127.0.0.1"}
