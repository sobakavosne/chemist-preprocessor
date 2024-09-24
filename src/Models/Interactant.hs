{-# LANGUAGE DeriveGeneric #-}

-- | Haskell models
module Models.Interactant where

import           Data.Aeson       (FromJSON, ToJSON)
import           GHC.Generics     (Generic)
import           Models.Chemical  (ACCELERATE, Catalyst, Molecule, PRODUCT_FROM,
                                   REAGENT_IN, Reaction)
import           Models.Mechanism (Mechanism, Stage)

-- | Primary type for Reaction explanation
data Interactant
  = IAccelerate ACCELERATE
  | ICatalyst Catalyst
  | IMolecule Molecule
  | IProductFrom PRODUCT_FROM
  | IReagentIn REAGENT_IN
  | IReaction Reaction
  deriving (Show, Generic, Eq)

instance ToJSON Interactant

instance FromJSON Interactant

-- | Secondary type for Mechanism explanation
data Explain
  = EMechanism Mechanism
  | EStage Stage
  deriving (Show, Generic, Eq)
