{-# LANGUAGE DuplicateRecordFields #-}

module DataTypes
  ( Molecule
  , Reaction
  , Catalyst
  , PRODUCT_FROM
  , ACCELERATE
  ) where

import           Data.Maybe (Maybe)

data Molecule =
  Molecule
    { id        :: Int
    , smiles    :: String
    , iupacName :: String
    }
  deriving (Show)

data Reaction =
  Reaction
    { id   :: Int
    , name :: String
    }
  deriving (Show)

data Catalyst =
  Catalyst
    { id     :: Int
    , smiles :: String
    , name   :: Maybe String
    }
  deriving (Show)

newtype PRODUCT_FROM =
  PRODUCT_FROM
    { amount :: Float
    }
  deriving (Show)

data ACCELERATE =
  ACCELERATE
    { temperature :: Float
    , pressure    :: Float
    }
  deriving (Show)
