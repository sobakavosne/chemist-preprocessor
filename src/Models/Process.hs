{-# LANGUAGE DeriveGeneric #-}

-- | Haskell models
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
