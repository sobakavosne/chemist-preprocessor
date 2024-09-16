{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Models where

import           Data.Aeson       (FromJSON, ToJSON, object, withObject, (.:),
                                   (.:?), (.=))
import           Data.Aeson.Key   (fromString)
import qualified Data.Aeson.Types
import           Data.Default     (Default (def))
import           Data.Map         (Map)
import           Data.Text        (Text)
import           Database.Bolt    (Node, Relationship, Value)
import           GHC.Generics     (Generic)

type ReactionID = Int

type MechanismID = Int

type MoleculeID = Int

type CatalystID = Int

type StageID = Int

-- | Represents a node in a reaction graph
type ReactionNode = Node

-- | Represents a node in a mechanism graph
type MechanismNode = Node

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

data Explain
  = EMechanism Mechanism
  | EStage Stage
  deriving (Show, Generic, Eq)

newtype NodeMask =
  NodeMask
    { nodePropsMask :: Map Text Value
    }
  deriving (Show, Eq)

newtype RelMask =
  RelMask
    { relPropsMask :: Map Text Value
    }
  deriving (Show, Eq)

data PathMask =
  PathMask
    { pathNodesMask         :: [Interactant]
    , pathRelationshipsMask :: [Interactant]
    , pathSequenceMask      :: [Int]
    }
  deriving (Show, Generic, Eq)

instance ToJSON PathMask

instance FromJSON PathMask

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

data Mechanism =
  Mechanism
    { mechanismId               :: MechanismID
    , mechanismName             :: String
    , mechanismType             :: String
    , mechanismActivationEnergy :: Float
    }
  deriving (Show, Generic, Eq)

instance FromJSON Mechanism

instance ToJSON Mechanism

newtype FOLLOW =
  FOLLOW
    { description :: String
    }
  deriving (Show, Generic, Eq)

instance FromJSON FOLLOW

instance ToJSON FOLLOW

data Stage =
  Stage
    { stageOrder       :: StageID
    , stageName        :: String
    , stageDescription :: String
    , stageProducts    :: [String]
    }
  deriving (Show, Generic, Eq)

instance FromJSON Stage

instance ToJSON Stage

data INCLUDE =
  INCLUDE
  deriving (Show, Generic, Eq)

instance FromJSON INCLUDE

instance ToJSON INCLUDE

data MechanismDetails =
  MechanismDetails
    { mechanismContext  :: (Mechanism, FOLLOW)
    , stageInteractants :: [(Stage, [Interactant])]
    }
  deriving (Show, Generic, Eq)

instance ToJSON MechanismDetails

instance FromJSON MechanismDetails

data RawMechanismDetails =
  RawMechanismDetails
    { rawMechanism    :: Node
    , rawInteractants :: [Node]
    , rawInclude      :: [Relationship]
    , rawStages       :: [Node]
    , rawFollow       :: Relationship
    }
  deriving (Show, Eq)

data RawMechanismDetailsMask =
  RawMechanismDetailsMask
    { rawMechanismMask    :: NodeMask
    , rawContextMask      :: RelMask
    , rawStagesMask       :: [NodeMask]
    , rawIncludeMask      :: [RelMask]
    , rawParticipantsMask :: [NodeMask]
    }
  deriving (Show, Eq)

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

data RawReactionDetailsMask =
  RawDetailsMask
    { rawReactionMask   :: NodeMask
    , rawReagentsMask   :: [NodeMask]
    , rawProductsMask   :: [NodeMask]
    , rawInboundMask    :: [RelMask]
    , rawOutboundMask   :: [RelMask]
    , rawAccelerateMask :: [RelMask]
    , rawCatalystsMask  :: [NodeMask]
    }
  deriving (Show, Eq)

data ProcessDetails =
  ProcessDetails
    { reactionDetails  :: ReactionDetails
    , mechanismDetails :: MechanismDetails
    }
  deriving (Show, Generic, Eq)

instance FromJSON ProcessDetails

instance ToJSON ProcessDetails

data HealthCheck =
  HealthCheck
    { status  :: String
    , message :: String
    }
  deriving (Show, Generic)

instance FromJSON HealthCheck

instance ToJSON HealthCheck
