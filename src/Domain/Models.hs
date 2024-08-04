{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Models
  ( Molecule(..)
  , Reaction(..)
  , Catalyst(..)
  , PRODUCT_FROM(..)
  , ACCELERATE(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)

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
    , name   :: Maybe String
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

data ACCELERATE =
  ACCELERATE
    { temperature :: Float
    , pressure    :: Float
    }
  deriving (Show, Generic)

instance FromJSON ACCELERATE

instance ToJSON ACCELERATE

newtype REAGENT_IN =
  REAGENT_IN
    { amount :: Float
    }
  deriving (Show, Generic)

instance FromJSON REAGENT_IN

instance ToJSON REAGENT_IN

data ReactionDetails =
  ReactionDetails
    { reaction   :: Reaction
    , reagents   :: [REAGENT_IN]
    , products   :: [PRODUCT_FROM]
    , catalyst   :: Maybe Catalyst
    , molecules  :: [Molecule]
    , accelerate :: Maybe ACCELERATE
    }
  deriving (Show, Generic)

instance FromJSON ReactionDetails

instance ToJSON ReactionDetails
