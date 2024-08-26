{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Models
  ( Molecule(..)
  , Reaction(..)
  , Catalyst(..)
  , PRODUCT_FROM(..)
  , REAGENT_IN(..)
  , ACCELERATE(..)
  , ReactionDetails(..)
  , RawReactionDetails(..)
  -- , Interactant(..)
  ) where

import           Data.Aeson    (FromJSON, ToJSON)

import           Data.Default  (Default (def))
import           Database.Bolt (Node, Relationship)
import           GHC.Generics  (Generic)
import           Prelude       hiding (id)

-- data Interactant
--   = IMolecule Molecule
--   | IReaction Reaction
--   | ICatalyst Catalyst
--   | IPRODUCT_FROM PRODUCT_FROM
--   | IREAGENT_IN REAGENT_IN
--   | IACCELERATE ACCELERATE
--   deriving (Eq, Show, Generic)
data Molecule =
  Molecule
    { id        :: Int
    , smiles    :: String
    , iupacName :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Molecule

instance ToJSON Molecule

data Reaction =
  Reaction
    { id   :: Int
    , name :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Reaction

instance ToJSON Reaction

data Catalyst =
  Catalyst
    { id     :: Int
    , smiles :: String
    , name   :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Catalyst

instance ToJSON Catalyst

newtype PRODUCT_FROM =
  PRODUCT_FROM
    { amount :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON PRODUCT_FROM

instance ToJSON PRODUCT_FROM

data ACCELERATE
  -- | `def` - the default ACCELERATE value corresponds to Standard Temperature and Pressure (STP):
  -- - temperature = 273.15 K (Kelvin)
  -- - pressure = 101.325 kPa (kilopascals)
      =
  ACCELERATE
    { temperature :: [Float]
    , pressure    :: [Float]
    }
  deriving (Show, Generic, Eq)

instance Default ACCELERATE where
  def :: ACCELERATE
  def = ACCELERATE {temperature = [273.15], pressure = [101.325]}

instance FromJSON ACCELERATE

instance ToJSON ACCELERATE

newtype REAGENT_IN =
  REAGENT_IN
    { amount :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON REAGENT_IN

instance ToJSON REAGENT_IN

data ReactionDetails =
  Details
    { reaction         :: Reaction
    , inboundReagents  :: [(Molecule, REAGENT_IN)]
    , outboundProducts :: [(Molecule, PRODUCT_FROM)]
    , conditions       :: [(Catalyst, ACCELERATE)]
    }
  deriving (Show, Generic)

instance FromJSON ReactionDetails

instance ToJSON ReactionDetails

data RawReactionDetails =
  RawDetails
    { reaction   :: Node
    , reagents   :: [Node]
    , products   :: [Node]
    , inbound    :: [Relationship]
    , outbound   :: [Relationship]
    , accelerate :: [Relationship]
    , catalysts  :: [Node]
    }
  deriving (Show, Eq)
