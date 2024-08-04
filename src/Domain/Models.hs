{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs          #-}

module Domain.Models
  ( Molecule(..)
  , Reaction(..)
  , Catalyst(..)
  , PRODUCT_FROM(..)
  , ACCELERATE(..)
  , ReactionDetails(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)

import           Data.Default (Default (def))
import           GHC.Generics (Generic)

data Molecule =
  Molecule
    { id        :: Int
    , smiles    :: String
    , iupacName :: String
    }
  deriving (Show, Generic)

instance FromJSON Molecule

instance ToJSON Molecule

data Reaction =
  Reaction
    { id   :: Int
    , name :: String
    }
  deriving (Show, Generic)

instance FromJSON Reaction

instance ToJSON Reaction

data Catalyst =
  Catalyst
    { id     :: Int
    , smiles :: String
    , name   :: String
    }
  deriving (Show, Generic)

instance FromJSON Catalyst

instance ToJSON Catalyst

newtype PRODUCT_FROM =
  PRODUCT_FROM
    { amount :: Float
    }
  deriving (Show, Generic)

instance FromJSON PRODUCT_FROM

instance ToJSON PRODUCT_FROM

data ACCELERATE
  -- | `def` - the default ACCELERATE value corresponds to Standard Temperature and Pressure (STP):
  -- - temperature = 273.15 K (Kelvin)
  -- - pressure = 101.325 kPa (kilopascals)
      =
  ACCELERATE
    { temperature :: Float
    , pressure    :: Float
    }
  deriving (Show, Generic)

instance Default ACCELERATE where
  def :: ACCELERATE
  def = ACCELERATE {temperature = 273.15, pressure = 101.325}

instance FromJSON ACCELERATE

instance ToJSON ACCELERATE

newtype REAGENT_IN =
  REAGENT_IN
    { amount :: Float
    }
  deriving (Show, Generic)

instance FromJSON REAGENT_IN

instance ToJSON REAGENT_IN

data Conditions =
  Conditions
    { catalyst   :: Maybe [Catalyst]
    , accelerate :: ACCELERATE
    }
  deriving (Show, Generic)

instance FromJSON Conditions

instance ToJSON Conditions

data ReactionDetails =
  ReactionDetails
    { reaction   :: Reaction
    , molecules  :: [Molecule]
    , inbound    :: [REAGENT_IN]
    , outbound   :: [PRODUCT_FROM]
    , conditions :: Conditions
    }
  deriving (Show, Generic)

instance FromJSON ReactionDetails

instance ToJSON ReactionDetails
