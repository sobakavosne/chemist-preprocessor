{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Models
  ( Molecule(..)
  , Reaction(..)
  , Catalyst(..)
  , PRODUCT_FROM(..)
  , REAGENT_IN(..)
  , ACCELERATE(..)
  , ReactionDetails(..)
  ) where

import           Data.Aeson    (FromJSON, ToJSON)

import           Data.Default  (Default (def))
import           Data.Text     (unpack)
import           Database.Bolt (RecordValue (exactEither), UnpackError (Not),
                                Value (N, S))
import           GHC.Generics  (Generic)
import           Helpers       (exactFields, fromDouble)
import           Prelude       hiding (id)

data Molecule =
  Molecule
    { id        :: Int
    , smiles    :: String
    , iupacName :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Molecule

instance ToJSON Molecule

instance RecordValue Molecule where
  exactEither :: Value -> Either UnpackError Molecule
  exactEither (S structure) = do
    id <- exactFields "id" structure
    smiles <- unpack <$> exactFields "smiles" structure
    iupacName <- unpack <$> exactFields "iupacName" structure
    return Molecule {id, smiles, iupacName}
  exactEither _ = Left $ Not "Molecule"

data Reaction =
  Reaction
    { id   :: Int
    , name :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Reaction

instance ToJSON Reaction

instance RecordValue Reaction where
  exactEither :: Value -> Either UnpackError Reaction
  exactEither (S structure) = do
    id <- exactFields "id" structure
    name <- unpack <$> exactFields "name" structure
    return Reaction {id, name}
  exactEither _ = Left $ Not "Reaction"

data Catalyst =
  Catalyst
    { id     :: Int
    , smiles :: String
    , name   :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Catalyst

instance ToJSON Catalyst

instance RecordValue Catalyst where
  exactEither :: Value -> Either UnpackError Catalyst
  exactEither (S structure) = do
    id <- exactFields "id" structure
    smiles <- unpack <$> exactFields "smiles" structure
    name <- unpack <$> exactFields "name" structure
    return Catalyst {id, smiles, name}
  exactEither _ = Left $ Not "Catalyst"

newtype PRODUCT_FROM =
  PRODUCT_FROM
    { amount :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON PRODUCT_FROM

instance ToJSON PRODUCT_FROM

instance RecordValue PRODUCT_FROM where
  exactEither :: Value -> Either UnpackError PRODUCT_FROM
  exactEither (S structure) = do
    amount <- fromDouble <$> exactFields "amount" structure
    return PRODUCT_FROM {amount}
  exactEither _ = Left $ Not "PRODUCT_FROM"

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

instance RecordValue ACCELERATE where
  exactEither :: Value -> Either UnpackError ACCELERATE
  exactEither (S structure) = do
    temperature <- map fromDouble <$> exactFields "temperature" structure
    pressure <- map fromDouble <$> exactFields "pressure" structure
    return ACCELERATE {temperature = temperature, pressure}
  exactEither (N _) = Right def
  exactEither _ = Left $ Not "ACCELERATE"

newtype REAGENT_IN =
  REAGENT_IN
    { amount :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON REAGENT_IN

instance ToJSON REAGENT_IN

instance RecordValue REAGENT_IN where
  exactEither :: Value -> Either UnpackError REAGENT_IN
  exactEither (S structure) = do
    amount <- fromDouble <$> exactFields "amount" structure
    return REAGENT_IN {amount}
  exactEither _ = Left $ Not "REAGENT_IN"

data ReactionDetails =
  ReactionDetails
    { reaction   :: Reaction
    , reagents   :: [Molecule]
    , products   :: [Molecule]
    , inbound    :: [REAGENT_IN]
    , outbound   :: [PRODUCT_FROM]
    , accelerate :: [ACCELERATE] -- list of all acceleration options
    , catalysts  :: [Catalyst]
    }
  deriving (Show, Generic)

instance FromJSON ReactionDetails

instance ToJSON ReactionDetails
