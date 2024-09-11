{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Domain.Converter.Instances where

import           Control.Exception     (throw)
import           Control.Monad         (forM)
import           Data.Bifunctor        (Bifunctor (first, second))
import           Data.Map.Strict       (Map, (!?))
import           Data.Text             (Text, pack, unpack)
import           Database.Bolt         (IsValue (..), Node (..), Path (..),
                                        Relationship (..), URelationship (..),
                                        Value (..), props)
import           Domain.Converter.Type (ElemInteractant (..), FromValue (..),
                                        Identity (..), InteractantElem (..),
                                        ParsingError (..),
                                        Subject (SNode, SPath, SRel, SURel))
import           Models                (ACCELERATE (..), Catalyst (..),
                                        FOLLOW (..), INCLUDE (..),
                                        Interactant (..), Mechanism (..),
                                        Molecule (..), NodeMask (..),
                                        PRODUCT_FROM (..), PathMask (..),
                                        REAGENT_IN (..), Reaction (..),
                                        RelMask (..), Stage (..))

import           Numeric.Extra         (doubleToFloat)

instance FromValue Int where
  fromValue (I i) = Right i

instance {-# OVERLAPPING #-} FromValue String where
  fromValue (T t) = (Right . unpack) t

instance FromValue Float where
  fromValue (F d) = Right (doubleToFloat d)

instance FromValue a => FromValue [a] where
  fromValue (L l) = mapM fromValue l

instance ElemInteractant (Molecule, Identity) where
  exactInteractant (SNode (Node {nodeIdentity, labels, nodeProps}))
    | "Molecule" `elem` labels = do
      moleculeId <- unpackProp "id" nodeProps
      moleculeSmiles <- unpackProp "smiles" nodeProps
      moleculeIupacName <- unpackProp "iupacName" nodeProps
      return
        ( Molecule {moleculeId, moleculeSmiles, moleculeIupacName}
        , NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Molecule' label"

instance ElemInteractant (Catalyst, Identity) where
  exactInteractant (SNode (Node {nodeIdentity, labels, nodeProps}))
    | "Catalyst" `elem` labels = do
      catalystId <- unpackProp "id" nodeProps
      catalystSmiles <- unpackProp "smiles" nodeProps
      catalystName <- unpackProp "name" nodeProps
      return
        ( Catalyst {catalystId, catalystSmiles, catalystName}
        , NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Catalyst' label"

instance ElemInteractant (Reaction, Identity) where
  exactInteractant (SNode (Node {nodeIdentity, labels, nodeProps}))
    | "Reaction" `elem` labels = do
      reactionId <- unpackProp "id" nodeProps
      reactionName <- unpackProp "name" nodeProps
      return (Reaction {reactionId, reactionName}, NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Reaction' label"

instance ElemInteractant (REAGENT_IN, Identity) where
  exactInteractant (SRel (Relationship {startNodeId, relProps})) = do
    reagentAmount <- unpackProp "amount" relProps
    return (REAGENT_IN {reagentAmount}, RelTargetNodeId startNodeId)
  exactInteractant (SURel (URelationship {urelIdentity, urelProps})) = do
    reagentAmount <- unpackProp "amount" urelProps
    return (REAGENT_IN {reagentAmount}, URelId urelIdentity)

instance ElemInteractant (PRODUCT_FROM, Identity) where
  exactInteractant (SRel (Relationship {endNodeId, relProps})) = do
    productAmount <- unpackProp "amount" relProps
    return (PRODUCT_FROM {productAmount}, RelTargetNodeId endNodeId)
  exactInteractant (SURel (URelationship {urelIdentity, urelProps})) = do
    productAmount <- unpackProp "amount" urelProps
    return (PRODUCT_FROM {productAmount}, URelId urelIdentity)

instance ElemInteractant (ACCELERATE, Identity) where
  exactInteractant (SRel (Relationship {startNodeId, relProps})) = do
    pressure <- unpackProp "pressure" relProps
    temperature <- unpackProp "temperature" relProps
    return (ACCELERATE {pressure, temperature}, RelTargetNodeId startNodeId)
  exactInteractant (SURel (URelationship {urelIdentity, urelProps})) = do
    pressure <- unpackProp "pressure" urelProps
    temperature <- unpackProp "temperature" urelProps
    return (ACCELERATE {pressure, temperature}, URelId urelIdentity)

instance ElemInteractant (Mechanism, Identity) where
  exactInteractant (SNode (Node {nodeIdentity, labels, nodeProps}))
    | "Mechanism" `elem` labels = do
      mechanismId <- unpackProp "id" nodeProps
      mechanismName <- unpackProp "name" nodeProps
      mechanismType <- unpackProp "type" nodeProps
      mechanismActivationEnergy <- unpackProp "activationEnergy" nodeProps
      return
        ( Mechanism
            { mechanismId
            , mechanismName
            , mechanismType
            , mechanismActivationEnergy
            }
        , NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Mechanism' label"

instance ElemInteractant (Stage, Identity) where
  exactInteractant (SNode (Node {nodeIdentity, labels, nodeProps}))
    | "Stage" `elem` labels = do
      stageOrder <- unpackProp "order" nodeProps
      stageName <- unpackProp "name" nodeProps
      stageDescription <- unpackProp "description" nodeProps
      stageProducts <- unpackProp "products" nodeProps
      return
        ( Stage {stageOrder, stageName, stageDescription, stageProducts}
        , NodeId nodeIdentity)
    | otherwise = throw $ ParsingError "No 'Stage' label"

-- "Many-to-many" correspondence interpretation for graph
instance ElemInteractant (INCLUDE, Identity, Identity) where
  exactInteractant (SRel (Relationship {startNodeId, endNodeId})) = do
    return (INCLUDE, RelStartNodeId startNodeId, RelTargetNodeId endNodeId)
  exactInteractant (SURel (URelationship {urelIdentity})) = do
    return (INCLUDE, URelId urelIdentity, URelId urelIdentity)

instance ElemInteractant (FOLLOW, Identity) where
  exactInteractant (SRel (Relationship {endNodeId, relProps})) = do
    description <- unpackProp "description" relProps
    return (FOLLOW {description}, RelTargetNodeId endNodeId)
  exactInteractant (SURel (URelationship {urelIdentity, urelProps})) = do
    description <- unpackProp "description" urelProps
    return (FOLLOW {description}, URelId urelIdentity)

instance ElemInteractant (Interactant, Identity) where
  exactInteractant element@(SNode (Node {labels})) = do
    case labels of
      ["Molecule"] ->
        (second . first)
          IMolecule
          (exactInteractant element :: Either ParsingError (Molecule, Identity))
      ["Catalyst"] ->
        (second . first)
          ICatalyst
          (exactInteractant element :: Either ParsingError (Catalyst, Identity))
      ["Reaction"] ->
        (second . first)
          IReaction
          (exactInteractant element :: Either ParsingError (Reaction, Identity))
      _ ->
        (throw . ParsingError . pack)
          ("Unrecognized Node labels: " ++ show labels)
  exactInteractant element@(SURel (URelationship {urelType})) = do
    case urelType of
      "ACCELERATE" ->
        (second . first)
          IAccelerate
          (exactInteractant element :: Either ParsingError ( ACCELERATE
                                                           , Identity))
      "REAGENT_IN" ->
        (second . first)
          IReagentIn
          (exactInteractant element :: Either ParsingError ( REAGENT_IN
                                                           , Identity))
      "PRODUCT_FROM" ->
        (second . first)
          IProductFrom
          (exactInteractant element :: Either ParsingError ( PRODUCT_FROM
                                                           , Identity))
      _ ->
        (throw . ParsingError . pack)
          ("Unrecognized URelationship type: " ++ show urelType)

-- Introduce PathMask instance to avoid introducing a new typeclass for a pseudo-collection of Interactants
instance ElemInteractant PathMask where
  exactInteractant (SPath (Path {pathNodes, pathRelationships, pathSequence})) = do
    pathNodesMask <- forM pathNodes (fmap fst . parseNode)
    pathRelationshipsMask <- forM pathRelationships (fmap fst . parseURel)
    return
      (PathMask
         {pathNodesMask, pathRelationshipsMask, pathSequenceMask = pathSequence})
    where
      parseNode ::
           ElemInteractant (a, Identity)
        => Node
        -> Either ParsingError (a, Identity)
      parseNode = exactInteractant . SNode
      parseURel ::
           ElemInteractant (a, Identity)
        => URelationship
        -> Either ParsingError (a, Identity)
      parseURel = exactInteractant . SURel

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
