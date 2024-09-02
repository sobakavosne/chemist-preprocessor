{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Domain.Converter.Type
  ( Interactant(..)
  , Identity(..)
  , NodeMask(..)
  , RelMask(..)
  , Elem(..)
  , exactRaw
  , exact
  ) where

import           Control.Exception (Exception, throwIO)
import           Data.Map.Strict   (Map, (!?))
import           Data.String       (IsString (fromString))
import           Data.Text         (Text, unpack)
import           Database.Bolt     (IsValue (toValue, toValueList), Node (..),
                                    Path, Relationship (..), URelationship,
                                    Value (..), props)
import           Models            (ACCELERATE (..), Catalyst (..),
                                    Molecule (..), NodeMask (..),
                                    PRODUCT_FROM (..), REAGENT_IN (..),
                                    Reaction (..), RelMask (..))
import           Prelude           hiding (id)

data Elem
  -- | `Bolt` subjects
  = ENode Node
  | ERel Relationship
  | EURel URelationship
  | EPath Path
  deriving (Show, Eq)

data Identity
  -- | `Bolt` nodes' identifiers
  = NodeId Int
  | RelTargetNodeId Int
  deriving (Show)

data Interactant
  = IAccelerate ACCELERATE
  | ICatalyst Catalyst
  | IMolecule Molecule
  | IProductFrom PRODUCT_FROM
  | IReagentIn REAGENT_IN
  | IReaction Reaction
  deriving (Show, Eq)

instance Eq Identity where
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

class ElemInteractant a where
  exactInteractant :: Elem -> Either ParsingError a

exact :: ElemInteractant a => Elem -> IO a
exact = either (throwIO . userError . show) pure . exactInteractant

instance ElemInteractant (Molecule, Identity) where
  exactInteractant (ENode (Node {nodeIdentity, labels, nodeProps}))
    | "Molecule" `elem` labels = do
      moleculeId <- unpackProp "id" nodeProps
      moleculeSmiles <- unpackProp "smiles" nodeProps
      moleculeIupacName <- unpackProp "iupacName" nodeProps
      return
        ( Molecule {moleculeId, moleculeSmiles, moleculeIupacName}
        , NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Molecule' label"

instance ElemInteractant (Catalyst, Identity) where
  exactInteractant (ENode (Node {nodeIdentity, labels, nodeProps}))
    | "Catalyst" `elem` labels = do
      catalystId <- unpackProp "id" nodeProps
      catalystSmiles <- unpackProp "smiles" nodeProps
      catalystName <- unpackProp "name" nodeProps
      return
        ( Catalyst {catalystId, catalystSmiles, catalystName}
        , NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Catalyst' label"

instance ElemInteractant (Reaction, Identity) where
  exactInteractant (ENode (Node {nodeIdentity, labels, nodeProps}))
    | "Reaction" `elem` labels = do
      reactionId <- unpackProp "id" nodeProps
      reactionName <- unpackProp "name" nodeProps
      return (Reaction {reactionId, reactionName}, NodeId nodeIdentity)
    | otherwise = Left $ ParsingError "No 'Reaction' label"

instance ElemInteractant (REAGENT_IN, Identity) where
  exactInteractant (ERel (Relationship {startNodeId, relProps})) = do
    reagentAmount <- unpackProp "amount" relProps
    return (REAGENT_IN {reagentAmount}, RelTargetNodeId startNodeId)

instance ElemInteractant (PRODUCT_FROM, Identity) where
  exactInteractant (ERel (Relationship {endNodeId, relProps})) = do
    productAmount <- unpackProp "amount" relProps
    return (PRODUCT_FROM {productAmount}, RelTargetNodeId endNodeId)

instance ElemInteractant (ACCELERATE, Identity) where
  exactInteractant (ERel (Relationship {startNodeId, relProps})) = do
    pressure <- unpackProp "pressure" relProps
    temperature <- unpackProp "temperature" relProps
    return (ACCELERATE {pressure, temperature}, RelTargetNodeId startNodeId)

class InteractantElem a where
  exactElem :: Interactant -> Either ParsingError a

exactRaw :: InteractantElem a => Interactant -> IO a
exactRaw = either (throwIO . userError . show) pure . exactElem

instance InteractantElem RelMask where
  exactElem interactant =
    case interactant of
      IAccelerate (ACCELERATE {pressure, temperature}) -> do
        return
          RelMask
            { relProperties =
                props
                  [ ("pressure", toValueList pressure)
                  , ("temperature", toValueList temperature)
                  ]
            }
      IProductFrom (PRODUCT_FROM {productAmount}) -> do
        return
          RelMask {relProperties = props [("amount", toValue productAmount)]}
      IReagentIn (REAGENT_IN {reagentAmount}) -> do
        return
          RelMask {relProperties = props [("amount", toValue reagentAmount)]}

instance InteractantElem NodeMask where
  exactElem interactant =
    case interactant of
      ICatalyst (Catalyst {catalystId, catalystSmiles, catalystName}) -> do
        return
          NodeMask
            { nodeProperties =
                props
                  [ ("id", toValue catalystId)
                  , ("smiles", toValue catalystSmiles)
                  , ("name", toValue catalystName)
                  ]
            }
      IMolecule (Molecule {moleculeId, moleculeSmiles, moleculeIupacName}) -> do
        return
          NodeMask
            { nodeProperties =
                props
                  [ ("id", toValue moleculeId)
                  , ("smiles", toValue moleculeSmiles)
                  , ("iupacName", toValue moleculeIupacName)
                  ]
            }
      IReaction (Reaction {reactionId, reactionName}) -> do
        return
          NodeMask
            { nodeProperties =
                props
                  [("id", toValue reactionId), ("name", toValue reactionName)]
            }

unpackProp :: FromValue a => Text -> Map Text Value -> Either ParsingError a
unpackProp key properties =
  case properties !? key of
    Just x -> fromValue x
    _      -> (Left . ParsingError) ("No key: " <> key)

fromDouble :: Double -> Float
fromDouble = realToFrac
