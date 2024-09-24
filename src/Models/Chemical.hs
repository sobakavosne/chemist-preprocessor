{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Haskell models
module Models.Chemical where

import           Data.Aeson       (FromJSON, ToJSON, object, withObject, (.:),
                                   (.:?), (.=))
import           Data.Aeson.Key   (fromString)
import qualified Data.Aeson.Types
import           Data.Default     (Default (def))
import           GHC.Generics     (Generic)

type ReactionID = Int

type MoleculeID = Int

type CatalystID = Int

data Molecule =
  Molecule
    { moleculeId        :: MoleculeID
    , moleculeSmiles    :: String
    , moleculeIupacName :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Molecule

instance ToJSON Molecule

data Reaction =
  Reaction
    { reactionId   :: ReactionID
    , reactionName :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON Reaction

instance ToJSON Reaction

data Catalyst =
  Catalyst
    { catalystId     :: CatalystID
    , catalystSmiles :: String
    , catalystName   :: Maybe String
    }
  deriving (Show, Generic, Eq)

instance Default Catalyst where
  def =
    Catalyst
      { catalystId = 0
      , catalystSmiles = "Default Catalyst"
      , catalystName = Nothing
      }

instance FromJSON Catalyst where
  parseJSON =
    withObject "Catalyst" $ \v ->
      Catalyst <$> v .: fromString "catalystId" <*>
      v .: fromString "catalystSmiles" <*>
      v .:? fromString "catalystName"

instance ToJSON Catalyst where
  toJSON (Catalyst {catalystId, catalystSmiles, catalystName}) =
    object $
    filter
      ((/=) Data.Aeson.Types.Null . snd)
      [ fromString "catalystId" .= catalystId
      , fromString "catalystSmiles" .= catalystSmiles
      , fromString "catalystName" .= catalystName
      ]

newtype PRODUCT_FROM =
  PRODUCT_FROM
    { productAmount :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON PRODUCT_FROM

instance ToJSON PRODUCT_FROM

-- | `def` - the default value for @ACCELERATE@ corresponds to Standard Temperature and Pressure (STP):
--   - `temperature` = 273.15 K (Kelvin)
--   - `pressure`    = 101.325 kPa (kilopascals)
data ACCELERATE =
  ACCELERATE
    { temperature :: [Float]
    , pressure    :: [Float]
    }
  deriving (Show, Generic, Eq)

instance Default ACCELERATE where
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

