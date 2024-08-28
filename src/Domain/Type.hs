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

import           Control.Exception    (Exception)
import           Control.Monad        (forM)
import           Control.Monad.Cont   (MonadIO (liftIO))
import           Control.Monad.Except (MonadError, throwError)
import           Data.Map.Strict      (Map, (!?))
import           Data.Text            (Text, unpack)
import           Database.Bolt        (Node (..), Path, Relationship (..),
                                       URelationship, Value (..))
import           Domain.Helpers       (fromDouble)
import           Models               (ACCELERATE (..), Catalyst (..),
                                       Molecule (..), PRODUCT_FROM (..),
                                       REAGENT_IN (..), RawReactionDetails (..),
                                       Reaction (..), ReactionDetails (..))
import           Prelude              hiding (id)

data GraphElem
  = NComponent Node
  | RComponent Relationship
  | UComponent URelationship
  | PComponent Path
  deriving (Show, Eq)

data Interactant
  = IMolecule Molecule
  | IReaction Reaction
  | ICatalyst Catalyst
  | IPRODUCT_FROM PRODUCT_FROM
  | IREAGENT_IN REAGENT_IN
  | IACCELERATE ACCELERATE
  deriving (Eq, Show)

data GraphElemIdentifier
  = NodeId Int
  | StartNodeId Int
  deriving (Show)

instance Eq GraphElemIdentifier where
  (NodeId x) == (NodeId y)           = x == y
  (StartNodeId x) == (StartNodeId y) = x == y
  (NodeId x) == (StartNodeId y)      = x == y
  (StartNodeId x) == (NodeId y)      = x == y

newtype ParsingError =
  ParsingError Text

instance Show ParsingError where
  show (ParsingError msg) = "Interactants parsing error: " <> show msg

instance Exception ParsingError

instance MonadIO (Either ParsingError) where
  liftIO = liftIO

class ValueInteractants a where
  exactEither :: GraphElem -> Either ParsingError a

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

exact :: (MonadError ParsingError m, ValueInteractants a) => GraphElem -> m a
exact = either throwError pure . exactEither

instance ValueInteractants (Molecule, GraphElemIdentifier) where
  exactEither (NComponent (Node {nodeIdentity, labels, nodeProps}))
    | "Molecule" `elem` labels = do
      id <- unpackProp "id" nodeProps
      smiles <- unpackProp "smiles" nodeProps
      iupacName <- unpackProp "iupacName" nodeProps
      return (Molecule {id, smiles, iupacName}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Molecule' label"

instance ValueInteractants (Catalyst, GraphElemIdentifier) where
  exactEither (NComponent (Node {nodeIdentity, labels, nodeProps}))
    | "Catalyst" `elem` labels = do
      id <- unpackProp "id" nodeProps
      smiles <- unpackProp "smiles" nodeProps
      name <- unpackProp "name" nodeProps
      return (Catalyst {id, smiles, name}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Catalyst' label"

instance ValueInteractants (Reaction, GraphElemIdentifier) where
  exactEither (NComponent (Node {nodeIdentity, labels, nodeProps}))
    | "Reaction" `elem` labels = do
      id <- unpackProp "id" nodeProps
      name <- unpackProp "name" nodeProps
      return (Reaction {id, name}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Reaction' label"

instance ValueInteractants (REAGENT_IN, GraphElemIdentifier) where
  exactEither (RComponent (Relationship {startNodeId, relProps})) = do
    amount <- unpackProp "amount" relProps
    return (REAGENT_IN {amount}, StartNodeId startNodeId)

instance ValueInteractants (PRODUCT_FROM, GraphElemIdentifier) where
  exactEither (RComponent (Relationship {startNodeId, relProps})) = do
    amount <- unpackProp "amount" relProps
    return (PRODUCT_FROM {amount}, StartNodeId startNodeId)

instance ValueInteractants (ACCELERATE, GraphElemIdentifier) where
  exactEither (RComponent (Relationship {startNodeId, relProps})) = do
    pressure <- unpackProp "pressure" relProps
    temperature <- unpackProp "temperature" relProps
    return (ACCELERATE {pressure, temperature}, StartNodeId startNodeId)

unpackProp :: FromValue a => Text -> Map Text Value -> Either ParsingError a
unpackProp key props =
  case props !? key of
    Just x -> fromValue x
    _      -> (Left . ParsingError) ("No key: " <> key)

-- Let's suppose we have a unique values
direction :: Eq b => [(a, b)] -> [(c, b)] -> [(a, c)]
direction xs ys = [(x1, y1) | (x1, x2) <- xs, (y1, y2) <- ys, x2 == y2]

convert :: MonadError ParsingError m => RawReactionDetails -> m ReactionDetails
convert RawDetails { reaction
                   , reagents
                   , products
                   , inbound
                   , outbound
                   , accelerate
                   , catalysts
                   } = do
  (reaction' :: (Reaction, GraphElemIdentifier)) <-
    (exact . NComponent) reaction
  (reagents' :: [(Molecule, GraphElemIdentifier)]) <-
    forM reagents (exact . NComponent)
  (products' :: [(Molecule, GraphElemIdentifier)]) <-
    forM products (exact . NComponent)
  (inbound' :: [(REAGENT_IN, GraphElemIdentifier)]) <-
    forM inbound (exact . RComponent)
  (outbound' :: [(PRODUCT_FROM, GraphElemIdentifier)]) <-
    forM outbound (exact . RComponent)
  (catalysts' :: [(Catalyst, GraphElemIdentifier)]) <-
    forM catalysts (exact . NComponent)
  (accelerate' :: [(ACCELERATE, GraphElemIdentifier)]) <-
    forM accelerate (exact . RComponent)
  return
    Details
      { reaction = fst reaction'
      , inboundReagents = direction inbound' reagents'
      , outboundProducts = direction outbound' products'
      , conditions = direction accelerate' catalysts'
      }
