{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Domain.Type
  ( ValueInteractants(..)
  , ParsingError(..)
  , convert
  , exact
  ) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad     (forM)
import           Data.Map.Strict   (Map, (!?))
import           Data.String       (IsString (fromString))
import           Data.Text         (Text, unpack)
import           Database.Bolt     (Node (..), Path, Relationship (..),
                                    URelationship, Value (..))
import           Models            (ACCELERATE (..), Catalyst (..),
                                    Molecule (..), PRODUCT_FROM (..),
                                    REAGENT_IN (..), RawReactionDetails (..),
                                    Reaction (..), ReactionDetails (..))
import           Prelude           hiding (id)

data GraphElem
  -- | `Bolt` subjects
  = NElem Node
  | RElem Relationship
  | UElem URelationship
  | PElem Path
  deriving (Show, Eq)

data GraphElemId
  -- | `Bolt` nodes' identifiers
  = NodeId Int
  | RelTargetNodeId Int
  deriving (Show)

instance Eq GraphElemId where
  (NodeId x) == (NodeId y)                   = x == y
  (RelTargetNodeId x) == (RelTargetNodeId y) = x == y
  (NodeId x) == (RelTargetNodeId y)          = x == y
  (RelTargetNodeId x) == (NodeId y)          = x == y

newtype ParsingError =
  ParsingError Text

instance Show ParsingError where
  show (ParsingError msg) = "Interactants parsing error: " <> show msg

instance Exception ParsingError

instance IsString ParsingError where
  fromString = fromString

class FromValue a where
  fromValue :: Value -> Either ParsingError a

instance FromValue Int where
  fromValue (I i) = Right i

instance {-# OVERLAPPING #-} FromValue String where
  fromValue (T t) = (Right . unpack) t

instance FromValue Float where
  fromValue (F d) = (Right . fromDouble) d

instance FromValue a => FromValue [a] where
  fromValue (L l) = mapM fromValue l

class ValueInteractants a where
  exactEither :: GraphElem -> Either ParsingError a

exact :: ValueInteractants a => GraphElem -> IO a
exact = either (throwIO . userError . show) pure . exactEither

instance ValueInteractants (Molecule, GraphElemId) where
  exactEither (NElem (Node {nodeIdentity, labels, nodeProps}))
    | "Molecule" `elem` labels = do
      id <- unpackProp "id" nodeProps
      smiles <- unpackProp "smiles" nodeProps
      iupacName <- unpackProp "iupacName" nodeProps
      return (Molecule {id, smiles, iupacName}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Molecule' label"

instance ValueInteractants (Catalyst, GraphElemId) where
  exactEither (NElem (Node {nodeIdentity, labels, nodeProps}))
    | "Catalyst" `elem` labels = do
      id <- unpackProp "id" nodeProps
      smiles <- unpackProp "smiles" nodeProps
      name <- unpackProp "name" nodeProps
      return (Catalyst {id, smiles, name}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Catalyst' label"

instance ValueInteractants (Reaction, GraphElemId) where
  exactEither (NElem (Node {nodeIdentity, labels, nodeProps}))
    | "Reaction" `elem` labels = do
      id <- unpackProp "id" nodeProps
      name <- unpackProp "name" nodeProps
      return (Reaction {id, name}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Reaction' label"

instance ValueInteractants (REAGENT_IN, GraphElemId) where
  exactEither (RElem (Relationship {startNodeId, relProps})) = do
    amount <- unpackProp "amount" relProps
    return (REAGENT_IN {amount}, RelTargetNodeId startNodeId)

instance ValueInteractants (PRODUCT_FROM, GraphElemId) where
  exactEither (RElem (Relationship {endNodeId, relProps})) = do
    amount <- unpackProp "amount" relProps
    return (PRODUCT_FROM {amount}, RelTargetNodeId endNodeId)

instance ValueInteractants (ACCELERATE, GraphElemId) where
  exactEither (RElem (Relationship {startNodeId, relProps})) = do
    pressure <- unpackProp "pressure" relProps
    temperature <- unpackProp "temperature" relProps
    return (ACCELERATE {pressure, temperature}, RelTargetNodeId startNodeId)

unpackProp :: FromValue a => Text -> Map Text Value -> Either ParsingError a
unpackProp key props =
  case props !? key of
    Just x -> fromValue x
    _      -> (Left . ParsingError) ("No key: " <> key)

-- Let's suppose we have a unique values
directionOf :: Eq b => [(a, b)] -> [(c, b)] -> [(a, c)]
directionOf xs ys = [(x1, y1) | (x1, x2) <- xs, (y1, y2) <- ys, x2 == y2]

fromDouble :: Double -> Float
fromDouble = realToFrac :: Double -> Float

convert :: RawReactionDetails -> IO ReactionDetails
convert RawDetails { reaction
                   , reagents
                   , products
                   , inbound
                   , outbound
                   , accelerate
                   , catalysts
                   } = do
  (reaction' :: (Reaction, GraphElemId)) <- (exact . NElem) reaction
  (reagents' :: [(Molecule, GraphElemId)]) <- forM reagents (exact . NElem)
  (products' :: [(Molecule, GraphElemId)]) <- forM products (exact . NElem)
  (inbound' :: [(REAGENT_IN, GraphElemId)]) <- forM inbound (exact . RElem)
  (outbound' :: [(PRODUCT_FROM, GraphElemId)]) <- forM outbound (exact . RElem)
  (catalysts' :: [(Catalyst, GraphElemId)]) <- forM catalysts (exact . NElem)
  (accelerate' :: [(ACCELERATE, GraphElemId)]) <- forM accelerate (exact . RElem)
  return
    Details
      { reaction = fst reaction'
      , inboundReagents = directionOf inbound' reagents'
      , outboundProducts = directionOf outbound' products'
      , conditions = directionOf accelerate' catalysts'
      }
