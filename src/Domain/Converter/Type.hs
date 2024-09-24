module Domain.Converter.Type where

import           Control.Exception (Exception, throwIO)
import           Data.Text         (Text)
import           Database.Bolt     (Node (..), Path (..), Relationship (..),
                                    URelationship (..), Value (..))
import           Models            (Interactant (..))
import           Prelude           hiding (id)

-- | The `Elem` data type represents various types of Bolt (Neo4j) elements that
--   can be extracted from the database. It includes:
--   * `SNode` for nodes.
--   * `SRel` for relationships.
--   * `SURel` for universal (undirected) relationships.
--   * `SPath` for paths.
data Elem
  = SNode Node
  | SRel Relationship
  | SURel URelationship
  | SPath Path
  deriving (Show, Eq)

-- | The `Identity` data type represents various IDs that can be associated with
--   nodes or relationships in the graph:
--   * `NodeId` represents a node's unique identifier.
--   * `URelId` represents universal (undirected) relationship's unique identifier.
--   * `RelStartNodeId` represents the starting node ID of a relationship.
--   * `RelTargetNodeId` represents the target node ID of a relationship.
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

-- | The `FromValue` type class is used to convert a `Value` (from the Bolt database)
--   into a specific Haskell type `a`. It includes two functions:
--   * `fromValue` attempts the conversion, returning an `Either` to handle errors.
--   * `maybeFromValue` attempts the conversion from a `Maybe Value`, returning `Nothing`
--     if the value is `Nothing` or the conversion fails.
class FromValue a where
  fromValue :: Value -> Either ParsingError a
  maybeFromValue :: Maybe Value -> Maybe a

-- | The `ElemInteractant` type class defines how to extract an interactant from a `Subject`.
--   It allows us to convert various database entities into interactants.
--
--   Used to read from DataBase.
class ElemInteractant a where
  exactInteractant :: Elem -> Either ParsingError a

-- | `exact` converts a `Subject` to an interactant by using the `exactInteractant` function.
--   It throws a `ParsingError` if the conversion fails, otherwise it returns the result.
--
--   Used to read from DataBase.
exact :: ElemInteractant a => Elem -> IO a
exact = either throwIO pure . exactInteractant

-- | The `InteractantElem` type class defines how to extract an element from an `Interactant`.
--   This allows specific parts of an `Interactant` to be converted into `Masks`.
--
--   Used to write into DataBase.
class InteractantElem a where
  exactElem :: Interactant -> Either ParsingError a

-- | `exactRaw` converts an `Interactant` to a specific element using `exactElem`.
--   Similar to `exact`, it throws a `ParsingError` if the conversion fails.
--
--   Used to write into DataBase.
exactRaw :: InteractantElem a => Interactant -> IO a
exactRaw = either throwIO pure . exactElem
