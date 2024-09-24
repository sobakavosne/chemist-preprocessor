{-# LANGUAGE DeriveGeneric #-}

-- | WEB masks
module Models.Mask where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Map           (Map)
import           Data.Text          (Text)
import           Database.Bolt      (Value)
import           GHC.Generics       (Generic)
import           Models.Interactant (Interactant)

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

data RawMechanismDetailsMask =
  RawMechanismDetailsMask
    { rawMechanismMask    :: NodeMask
    , rawContextMask      :: RelMask
    , rawStagesMask       :: [NodeMask]
    , rawIncludeMask      :: [RelMask]
    , rawParticipantsMask :: [NodeMask]
    }
  deriving (Show, Eq)

data RawReactionDetailsMask =
  RawReactionDetailsMask
    { rawReactionMask   :: NodeMask
    , rawReagentsMask   :: [NodeMask]
    , rawProductsMask   :: [NodeMask]
    , rawInboundMask    :: [RelMask]
    , rawOutboundMask   :: [RelMask]
    , rawAccelerateMask :: [RelMask]
    , rawCatalystsMask  :: [NodeMask]
    }
  deriving (Show, Eq)
