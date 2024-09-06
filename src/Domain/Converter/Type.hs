{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Domain.Converter.Type
  ( Interactant(..)
  , Identity(..)
  , Elem(..)
  , exactRaw
  , exact
  ) where

import           Control.Exception (Exception, throw, throwIO)
import           Control.Monad     (forM)
import           Data.Map.Strict   (Map, (!?))
import           Data.Text         (Text, pack, unpack)
import           Database.Bolt     (IsValue (toValue, toValueList), Node (..),
                                    Path (..), Relationship (..),
                                    URelationship (..), Value (..), props)
import           Models            (ACCELERATE (..), Catalyst (..),
                                    Interactant (..), Molecule (..),
                                    NodeMask (..), PRODUCT_FROM (..),
                                    PathMask (..), REAGENT_IN (..),
                                    Reaction (..), RelMask (..))
import           Numeric.Extra     (doubleToFloat)
import           Prelude           hiding (id)

data Elem
  -- | `Bolt` subjects
  = ENode Node
  | ERel Relationship
  | EURel URelationship
  | EPath Path
  deriving (Show, Eq)

data Identity
  -- | `Bolt` elements identifiers
  = NodeId Int
  | URelId Int
  | RelTargetNodeId Int
  deriving (Show)

instance Eq Identity where
  (NodeId x) == (NodeId y)                   = x == y
  (RelTargetNodeId x) == (RelTargetNodeId y) = x == y
  (NodeId x) == (RelTargetNodeId y)          = x == y
  (RelTargetNodeId x) == (NodeId y)          = x == y

newtype ParsingError =
  ParsingError Text

instance Show ParsingError where
  show (ParsingError msg) = "Parsing error: " <> show msg

instance Exception ParsingError

class FromValue a where
  fromValue :: Value -> Either ParsingError a

instance FromValue Int where
  fromValue (I i) = Right i

instance {-# OVERLAPPING #-} FromValue String where
  fromValue (T t) = (Right . unpack) t

instance FromValue Float where
  fromValue (F d) = Right (doubleToFloat d)

instance FromValue a => FromValue [a] where
  fromValue (L l) = mapM fromValue l

class ElemInteractant a where
  exactInteractant :: Elem -> Either ParsingError a

exact :: ElemInteractant a => Elem -> IO a
exact = either throwIO pure . exactInteractant

instance ElemInteractant (Molecule, Identity) where
  exactInteractant (ENode (Node {nodeIdentity, labels, nodeProps}))
    | "Molecule" `elem` labels = do
      moleculeId <- unpackProp "id" nodeProps
      moleculeSmiles <- unpackProp "smiles" nodeProps
      moleculeIupacName <- unpackProp "iupacName" nodeProps
      return
        ( Molecule {moleculeId, moleculeSmiles, moleculeIupacName}
        , NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Molecule' label"

instance ElemInteractant (Catalyst, Identity) where
  exactInteractant (ENode (Node {nodeIdentity, labels, nodeProps}))
    | "Catalyst" `elem` labels = do
      catalystId <- unpackProp "id" nodeProps
      catalystSmiles <- unpackProp "smiles" nodeProps
      catalystName <- unpackProp "name" nodeProps
      return
        ( Catalyst {catalystId, catalystSmiles, catalystName}
        , NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Catalyst' label"

instance ElemInteractant (Reaction, Identity) where
  exactInteractant (ENode (Node {nodeIdentity, labels, nodeProps}))
    | "Reaction" `elem` labels = do
      reactionId <- unpackProp "id" nodeProps
      reactionName <- unpackProp "name" nodeProps
      return (Reaction {reactionId, reactionName}, NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Reaction' label"

instance ElemInteractant (REAGENT_IN, Identity) where
  exactInteractant (ERel (Relationship {startNodeId, relProps})) = do
    reagentAmount <- unpackProp "amount" relProps
    return (REAGENT_IN {reagentAmount}, RelTargetNodeId startNodeId)
  exactInteractant (EURel (URelationship {urelIdentity, urelProps})) = do
    reagentAmount <- unpackProp "amount" urelProps
    return (REAGENT_IN {reagentAmount}, URelId urelIdentity)

instance ElemInteractant (PRODUCT_FROM, Identity) where
  exactInteractant (ERel (Relationship {endNodeId, relProps})) = do
    productAmount <- unpackProp "amount" relProps
    return (PRODUCT_FROM {productAmount}, RelTargetNodeId endNodeId)
  exactInteractant (EURel (URelationship {urelIdentity, urelProps})) = do
    productAmount <- unpackProp "amount" urelProps
    return (PRODUCT_FROM {productAmount}, URelId urelIdentity)

instance ElemInteractant (ACCELERATE, Identity) where
  exactInteractant (ERel (Relationship {startNodeId, relProps})) = do
    pressure <- unpackProp "pressure" relProps
    temperature <- unpackProp "temperature" relProps
    return (ACCELERATE {pressure, temperature}, RelTargetNodeId startNodeId)
  exactInteractant (EURel (URelationship {urelIdentity, urelProps})) = do
    pressure <- unpackProp "pressure" urelProps
    temperature <- unpackProp "temperature" urelProps
    return (ACCELERATE {pressure, temperature}, URelId urelIdentity)

instance ElemInteractant Interactant where
  exactInteractant element@(ENode (Node {labels})) = do
    case labels of
      ["Molecule"] ->
        IMolecule . fst <$>
        (exactInteractant element :: Either ParsingError (Molecule, Identity))
      ["Catalyst"] ->
        ICatalyst . fst <$>
        (exactInteractant element :: Either ParsingError (Catalyst, Identity))
      ["Reaction"] ->
        IReaction . fst <$>
        (exactInteractant element :: Either ParsingError (Reaction, Identity))
      _ ->
        (throw . ParsingError . pack)
          ("Unrecognized Node labels: " ++ show labels)
  exactInteractant element@(EURel (URelationship {urelType})) = do
    case urelType of
      "ACCELERATE" ->
        IAccelerate . fst <$>
        (exactInteractant element :: Either ParsingError (ACCELERATE, Identity))
      "REAGENT_IN" ->
        IReagentIn . fst <$>
        (exactInteractant element :: Either ParsingError (REAGENT_IN, Identity))
      "PRODUCT_FROM" ->
        IProductFrom . fst <$>
        (exactInteractant element :: Either ParsingError ( PRODUCT_FROM
                                                         , Identity))
      _ ->
        (throw . ParsingError . pack)
          ("Unrecognized URelationship type: " ++ show urelType)

-- Introduce PathMask instance to avoid introducing a new typeclass for a pseudo-collection of Interactants
instance ElemInteractant PathMask where
  exactInteractant (EPath (Path {pathNodes, pathRelationships, pathSequence})) = do
    pathNodesMask <- forM pathNodes (exactInteractant . ENode)
    pathRelationshipsMask <- forM pathRelationships (exactInteractant . EURel)
    return
      (PathMask
         {pathNodesMask, pathRelationshipsMask, pathSequenceMask = pathSequence})

class InteractantElem a where
  exactElem :: Interactant -> Either ParsingError a

exactRaw :: InteractantElem a => Interactant -> IO a
exactRaw = either throwIO pure . exactElem

instance InteractantElem RelMask where
  exactElem interactant =
    case interactant of
      IAccelerate (ACCELERATE {pressure, temperature}) -> do
        return
          RelMask
            { relPropsMask =
                props
                  [ ("pressure", toValueList pressure)
                  , ("temperature", toValueList temperature)
                  ]
            }
      IProductFrom (PRODUCT_FROM {productAmount}) -> do
        return
          RelMask {relPropsMask = props [("amount", toValue productAmount)]}
      IReagentIn (REAGENT_IN {reagentAmount}) -> do
        return
          RelMask {relPropsMask = props [("amount", toValue reagentAmount)]}
      r ->
        (throw . ParsingError . pack)
          ("Unrecognized 'relation' Interactant: " ++ show r)

instance InteractantElem NodeMask where
  exactElem interactant =
    case interactant of
      ICatalyst (Catalyst {catalystId, catalystSmiles, catalystName}) -> do
        return
          NodeMask
            { nodePropsMask =
                props
                  [ ("id", toValue catalystId)
                  , ("smiles", toValue catalystSmiles)
                  , ("name", toValue catalystName)
                  ]
            }
      IMolecule (Molecule {moleculeId, moleculeSmiles, moleculeIupacName}) -> do
        return
          NodeMask
            { nodePropsMask =
                props
                  [ ("id", toValue moleculeId)
                  , ("smiles", toValue moleculeSmiles)
                  , ("iupacName", toValue moleculeIupacName)
                  ]
            }
      IReaction (Reaction {reactionId, reactionName}) -> do
        return
          NodeMask
            { nodePropsMask =
                props
                  [("id", toValue reactionId), ("name", toValue reactionName)]
            }
      n ->
        (throw . ParsingError . pack)
          ("Unrecognized 'node' Interactant: " ++ show n)

unpackProp :: FromValue a => Text -> Map Text Value -> Either ParsingError a
unpackProp key properties =
  case properties !? key of
    Just x -> fromValue x
    _      -> throw $ ParsingError ("Missing the key: " <> key)
