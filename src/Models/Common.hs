{-# LANGUAGE DeriveGeneric #-}

module Models.Common where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data HealthCheck =
  HealthCheck
    { status  :: String
    , message :: String
    }
  deriving (Show, Generic)

instance FromJSON HealthCheck

instance ToJSON HealthCheck
