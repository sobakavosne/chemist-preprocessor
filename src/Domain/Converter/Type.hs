-- | This module defines types and functions for converting Bolt (Neo4j)
--   elements and interactants into Haskell types.
--
--   ==== Base
--
--   * `exact` - Converts an `Elem` (such as nodes or relationships) into an
--     `Interactant` (and can be extend with `Explain` or other interactant types).
--     It handles errors by throwing a `ParsingError` if the conversion fails.
--
--   * `exactRaw` - Converts an `Interactant` (or any type, i.o. `Explain`) into a
--     specific `Elem`. It handles errors by throwing a `ParsingError` if the
--     conversion fails.
--
--   ==== Additional
--
--   * Data types for representing Neo4j elements and identities, where the
--     `Identity` type is introduced to facilitate the mapping of Neo4j objects
--     and building relationships.
--
--   * Type classes for converting from database values and extracting
--     interactants.
--
module Domain.Converter.Type where

import           Control.Exception (Exception, throwIO)
import           Data.Text         (Text)
import           Database.Bolt     (Node (..), Path (..), Relationship (..),
                                    URelationship (..), Value (..))
import           Models            (Interactant (..))
import           Prelude           hiding (id)

-- | The `Elem` data type represents various types of Bolt (Neo4j) elements that
--   can be extracted from the database. It includes:
--
-- ==== Constructors
--
-- * `SNode` - Represents nodes.
-- * `SRel` - Represents relationships.
-- * `SURel` - Represents universal (undirected) relationships.
-- * `SPath` - Represents paths.
data Elem
  = SNode Node
  | SRel Relationship
  | SURel URelationship
  | SPath Path
  deriving (Show, Eq)

-- | The `Identity` data type represents various IDs that can be associated with
--   nodes or relationships in the graph:
--
-- ==== Constructors
--
-- * `NodeId` - Represents a node's unique identifier.
-- * `URelId` - Represents a universal (undirected) relationship's unique identifier.
-- * `RelStartNodeId` - Represents the starting node ID of a relationship.
-- * `RelTargetNodeId` - Represents the target node ID of a relationship.
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
--   into a specific Haskell type `a`.
class FromValue a
  -- | Attempts to convert a `Value` to a specific Haskell type `a`.
  --
  --   This function takes a `Value` from the Bolt database and attempts to convert
  --   it to the desired type. If the conversion fails, it returns a `ParsingError`.
  --   Otherwise, it returns the converted value.
  where
  fromValue :: Value -> Either ParsingError a
  -- | Attempts to convert a `Maybe Value` to a specific Haskell type `a`.
  --
  --   This function takes an optional `Value` and attempts
  --   to convert it to the desired type.
  maybeFromValue :: Maybe Value -> Maybe a

-- | The `ElemInteractant` type class defines how to extract an interactant from a `Subject`.
--   It allows us to convert various database entities into interactants.
--
-- ==== Usage
--
-- This type class is used to read from the database.
class ElemInteractant a
  -- | Converts an `Elem` to an interactant of type `a`.
  --
  --   This function takes an `Elem` (which can be a node, relationship, etc.)
  --   and attempts to extract an interactant of type `a`.
  where
  exactInteractant :: Elem -> Either ParsingError a

-- | The `InteractantElem` type class defines how to extract an element from an `Interactant`.
--   This allows specific parts of an `Interactant` to be converted into `Masks`.
--
-- ==== Usage
--
-- This function is used to write into the database.
class InteractantElem a
  -- | Converts an `Interactant` to a specific type `a`.
  --
  --   This function takes an `Interactant` and attempts to extract a specific
  --   element of type `a` from it.
  where
  exactElem :: Interactant -> Either ParsingError a

-- | `exact` converts a `Subject` to an interactant by using the `exactInteractant` function.
--   It throws a `ParsingError` if the conversion fails; otherwise, it returns the result.
--
-- ==== Usage
--
-- This function is used to read from the database.
exact :: ElemInteractant a => Elem -> IO a
exact = either throwIO pure . exactInteractant

-- | `exactRaw` converts an `Interactant` to a specific element using `exactElem`.
--   Similar to `exact`, it throws a `ParsingError` if the conversion fails.
--
-- ==== Usage
--
-- This function is used to write into the database.
exactRaw :: InteractantElem a => Interactant -> IO a
exactRaw = either throwIO pure . exactElem
