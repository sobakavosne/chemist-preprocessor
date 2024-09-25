{-# LANGUAGE DeriveGeneric #-}

-- | **Models.Mechanism**
--
-- This module defines the `Mechanism`, `FOLLOW`, `Stage`, 
-- and `INCLUDE` data types for representing chemical mechanisms 
-- and their stages.
--
-- The `Mechanism` type encapsulates the essential properties 
-- of a mechanism, including its ID, name, type, and activation 
-- energy. The `FOLLOW` type allows for descriptive annotations 
-- related to the mechanism. The `Stage` type models the stages 
-- within a mechanism, including order, name, description, and 
-- associated products. The `INCLUDE` type is defined as a
-- placeholder for potential future extensions, enabling further
-- development of the model without breaking existing 
-- functionality.
--
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
