{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Models
  ( RelMask(..)
  , NodeMask(..)
  , Molecule(..)
  , Reaction(..)
  , Catalyst(..)
  , REAGENT_IN(..)
  , ACCELERATE(..)
  , PRODUCT_FROM(..)
  , ReactionDetails(..)
  , RawReactionDetails(..)
  , RawReactionDetailsMask(..)
  ) where

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Default  (Default (def))
import           Data.Map      (Map)
import           Data.Text     (Text)
import           Database.Bolt (Node, Relationship, Value)
import           GHC.Generics  (Generic)
import           Prelude       hiding (id)

newtype NodeMask =
  NodeMask
    { nodeProperties :: Map Text Value
    }
  deriving (Show, Eq)

newtype RelMask =
  RelMask
    { relProperties :: Map Text Value
    }
  deriving (Show, Eq)

data Molecule =
  Molecule
    { moleculeId        :: Int
    , moleculeSmiles    :: String
    , moleculeIupacName :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Molecule

instance ToJSON Molecule

data Reaction =
  Reaction
    { reactionId   :: Int
    , reactionName :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Reaction

instance ToJSON Reaction

data Catalyst =
  Catalyst
    { catalystId     :: Int
    , catalystSmiles :: String
    , catalystName   :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Catalyst

instance ToJSON Catalyst

newtype PRODUCT_FROM =
  PRODUCT_FROM
    { productAmount :: Float
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
    { reagentAmount :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON REAGENT_IN

instance ToJSON REAGENT_IN

data ReactionDetails =
  Details
    { reaction         :: Reaction
    , inboundReagents  :: [(REAGENT_IN, Molecule)]
    , outboundProducts :: [(PRODUCT_FROM, Molecule)]
    , conditions       :: [(ACCELERATE, Catalyst)]
    }
  deriving (Show, Generic, Eq)

instance FromJSON ReactionDetails

instance ToJSON ReactionDetails

data RawReactionDetails =
  RawDetails
    { rawReaction   :: Node
    , rawReagents   :: [Node]
    , rawProducts   :: [Node]
    , rawInbound    :: [Relationship]
    , rawOutbound   :: [Relationship]
    , rawAccelerate :: [Relationship]
    , rawCatalysts  :: [Node]
    }
  deriving (Show, Eq)

data RawReactionDetailsMask =
  RawDetailsMask
    { rawReactionMask   :: NodeMask
    , rawReagentsMask   :: [NodeMask]
    , rawProductsMask   :: [NodeMask]
    , rawInboundMask    :: [RelMask]
    , rawOutboundMask   :: [RelMask]
    , rawAccelerateMask :: [RelMask]
    , rawCatalystsMask  :: [NodeMask]
    }
  deriving (Show, Eq)
