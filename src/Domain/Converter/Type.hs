module Domain.Converter.Type where

import           Control.Exception (Exception, throwIO)
import           Data.Text         (Text)
import           Database.Bolt     (Node (..), Path (..), Relationship (..),
                                    URelationship (..), Value (..))
import           Models            (Interactant (..))
import           Prelude           hiding (id)

-- | `Bolt` subjects
data Subject
  = SNode Node
  | SRel Relationship
  | SURel URelationship
  | SPath Path
  deriving (Show, Eq)

-- | `Bolt` subjects identifiers
data Identity
  = NodeId Int
  | URelId Int
  | RelStartNodeId Int
  | RelTargetNodeId Int
  deriving (Show, Ord)

instance Eq Identity where
  (NodeId x) == (NodeId y)                   = x == y
  (NodeId x) == (URelId y)                   = x == y
  (NodeId x) == (RelStartNodeId y)           = x == y
  (NodeId x) == (RelTargetNodeId y)          = x == y
  (URelId x) == (NodeId y)                   = x == y
  (URelId x) == (URelId y)                   = x == y
  (URelId x) == (RelStartNodeId y)           = x == y
  (URelId x) == (RelTargetNodeId y)          = x == y
  (RelStartNodeId x) == (NodeId y)           = x == y
  (RelStartNodeId x) == (URelId y)           = x == y
  (RelStartNodeId x) == (RelStartNodeId y)   = x == y
  (RelStartNodeId x) == (RelTargetNodeId y)  = x == y
  (RelTargetNodeId x) == (NodeId y)          = x == y
  (RelTargetNodeId x) == (URelId y)          = x == y
  (RelTargetNodeId x) == (RelStartNodeId y)  = x == y
  (RelTargetNodeId x) == (RelTargetNodeId y) = x == y

newtype ParsingError =
  ParsingError Text

instance Show ParsingError where
  show (ParsingError msg) = "Parsing error: " <> show msg

instance Exception ParsingError

class FromValue a where
  fromValue :: Value -> Either ParsingError a

class ElemInteractant a where
  exactInteractant :: Subject -> Either ParsingError a

exact :: ElemInteractant a => Subject -> IO a
exact = either throwIO pure . exactInteractant

class InteractantElem a where
  exactElem :: Interactant -> Either ParsingError a

exactRaw :: InteractantElem a => Interactant -> IO a
exactRaw = either throwIO pure . exactElem
