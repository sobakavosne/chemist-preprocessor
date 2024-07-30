{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DataTypes
  ( Molecule(..)
  , Reaction(..)
  , Catalyst(..)
  , PRODUCT_FROM(..)
  , ACCELERATE(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)

-- import           Database.Bolt.Extras (Label)
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

-- instance Label Molecule where
--   label =  "Molecule"
--
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
