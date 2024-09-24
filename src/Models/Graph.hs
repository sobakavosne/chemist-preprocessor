module Models.Graph where

-- Bolt models
import           Database.Bolt (Node, Relationship)

-- | Represents a node in a reaction graph
type ReactionNode = Node

-- | Represents a node in a mechanism graph
type MechanismNode = Node

data RawReactionDetails =
  RawReactionDetails
    { rawReaction   :: Node
    , rawReagents   :: [Node]
    , rawProducts   :: [Node]
    , rawInbound    :: [Relationship]
    , rawOutbound   :: [Relationship]
    , rawAccelerate :: [Relationship]
    , rawCatalysts  :: [Node]
    }
  deriving (Show, Eq)

data RawMechanismDetails =
  RawMechanismDetails
    { rawMechanism    :: Node
    , rawInteractants :: [Node]
    , rawInclude      :: [Relationship]
    , rawStages       :: [Node]
    , rawFollow       :: Relationship
    }
  deriving (Show, Eq)
