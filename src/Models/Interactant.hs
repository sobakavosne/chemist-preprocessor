{-# LANGUAGE DeriveGeneric #-}

-- | This module defines the `Interactant` and `Explain` data types 
--   for representing various types of chemical entities and mechanisms 
--   in Haskell. The `Interactant` type encapsulates different forms of 
--   interactions, such as accelerations, catalysts, molecules, products, 
--   reagents, and reactions. The `Explain` type captures mechanisms 
--   and stages in a chemical process, providing a structured way to 
--   represent and serialize these entities in JSON format.
--
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

-- | Primary type for Mechanism explanation
data Explain
  = EMechanism Mechanism
  | EStage Stage
  deriving (Show, Generic, Eq)
