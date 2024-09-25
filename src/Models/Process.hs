{-# LANGUAGE DeriveGeneric #-}

-- | This module defines the `ReactionDetails`, `MechanismDetails`, 
--   and `ProcessDetails` data types for representing the details 
--   of chemical reactions and their associated mechanisms.
--
--   The `ReactionDetails` type encapsulates information about a 
--   chemical reaction, including the reaction itself, inbound 
--   reagents (as pairs of `REAGENT_IN` and `Molecule`), outbound 
--   products (as pairs of `PRODUCT_FROM` and `Molecule`), and 
--   relevant conditions (as pairs of `ACCELERATE` and `Catalyst`).
--
--   The `MechanismDetails` type models the context of a mechanism 
--   (including its description via `FOLLOW`) and the interactants 
--   associated with each stage of the mechanism.
--
--   Finally, the `ProcessDetails` type aggregates both `ReactionDetails` 
--   and `MechanismDetails`, providing a comprehensive overview 
--   of a chemical process.
--
module Models.Process where

import           Data.Aeson         (FromJSON, ToJSON)
import           GHC.Generics       (Generic)
import           Models.Chemical    (ACCELERATE, Catalyst, Molecule,
                                     PRODUCT_FROM, REAGENT_IN, Reaction)
import           Models.Interactant (Interactant)
import           Models.Mechanism   (FOLLOW, Mechanism, Stage)

data ReactionDetails =
  ReactionDetails
    { reaction         :: Reaction
    , inboundReagents  :: [(REAGENT_IN, Molecule)]
    , outboundProducts :: [(PRODUCT_FROM, Molecule)]
    , conditions       :: [(ACCELERATE, Catalyst)]
    }
  deriving (Show, Generic, Eq)

instance FromJSON ReactionDetails

instance ToJSON ReactionDetails

data MechanismDetails =
  MechanismDetails
    { mechanismContext  :: (Mechanism, FOLLOW)
    , stageInteractants :: [(Stage, [Interactant])]
    }
  deriving (Show, Generic, Eq)

instance ToJSON MechanismDetails

instance FromJSON MechanismDetails

data ProcessDetails =
  ProcessDetails
    { reactionDetails  :: ReactionDetails
    , mechanismDetails :: MechanismDetails
    }
  deriving (Show, Generic, Eq)

instance FromJSON ProcessDetails

instance ToJSON ProcessDetails
