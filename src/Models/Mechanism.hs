{-# LANGUAGE DeriveGeneric #-}

-- | Haskell models
module Models.Mechanism where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

type MechanismID = Int

type StageID = Int

data Mechanism =
  Mechanism
    { mechanismId               :: MechanismID
    , mechanismName             :: String
    , mechanismType             :: String
    , mechanismActivationEnergy :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON Mechanism

instance ToJSON Mechanism

newtype FOLLOW =
  FOLLOW
    { description :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON FOLLOW

instance ToJSON FOLLOW

data Stage =
  Stage
    { stageOrder       :: StageID
    , stageName        :: String
    , stageDescription :: String
    , stageProducts    :: [String]
    }
  deriving (Show, Generic, Eq)

instance FromJSON Stage

instance ToJSON Stage

data INCLUDE =
  INCLUDE
  deriving (Show, Generic, Eq)

instance FromJSON INCLUDE

instance ToJSON INCLUDE
