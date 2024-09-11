{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.QueryGenerator
  ( createReactionQueryFrom
  ) where

import           Data.Map      (Map, delete, toList, (!?))
import           Data.Text     (Text, intercalate, pack)
import           Database.Bolt (Value (..))
import           Models        (NodeMask (..), RawReactionDetailsMask (..),
                                RelMask (..))
import           Prelude       hiding (product)

createReactionQueryFrom :: RawReactionDetailsMask -> Text
createReactionQueryFrom details =
  let createNodes =
        "CREATE \n" <>
        nodeToCypher "reaction" "Reaction" (rawReactionMask details) <>
        ",\n" <>
        intercalate
          ",\n"
          (zipWith
             (\i reagent ->
                nodeToCypher
                  ("reagent" <> pack (show (i :: Int)))
                  "Molecule"
                  reagent)
             [1 ..]
             (rawReagentsMask details)) <>
        ",\n" <>
        intercalate
          ",\n"
          (zipWith
             (\i product ->
                nodeToCypher
                  ("product" <> pack (show (i :: Int)))
                  "Molecule"
                  product)
             [1 ..]
             (rawProductsMask details)) <>
        ",\n" <>
        intercalate
          "\n"
          (zipWith
             (\i catalyst ->
                nodeToCypher
                  ("catalyst" <> pack (show (i :: Int)))
                  "Catalyst"
                  catalyst)
             [1 ..]
             (rawCatalystsMask details)) <>
        ","
      createRelationships =
        intercalate ",\n" $
        zipWith
          (\i rel ->
             relToCypher
               ("reagent" <> pack (show (i :: Int)))
               "reaction"
               "REAGENT_IN"
               rel)
          [1 ..]
          (rawInboundMask details) <>
        zipWith
          (\i rel ->
             relToCypher
               "reaction"
               ("product" <> pack (show (i :: Int)))
               "PRODUCT_FROM"
               rel)
          [1 ..]
          (rawOutboundMask details) <>
        zipWith
          (\i rel ->
             relToCypher
               ("catalyst" <> pack (show (i :: Int)))
               "reaction"
               "ACCELERATE"
               rel)
          [1 ..]
          (rawAccelerateMask details)
   in intercalate "\n" [createNodes, createRelationships, "RETURN reaction;"]

nodeToCypher :: Text -> Text -> NodeMask -> Text
nodeToCypher alias label (NodeMask props) =
  let props' =
        case label of
          "Catalyst" -> checkCatalystName props
          _          -> props
   in "(" <> alias <> ":" <> label <> " {" <> propsToCypher props' <> "})"

checkCatalystName :: Map Text Value -> Map Text Value
checkCatalystName props =
  case props !? "name" of
    Just (N ()) -> delete "name" props
    _           -> props

relToCypher :: Text -> Text -> Text -> RelMask -> Text
relToCypher from to relType (RelMask props) =
  "(" <>
  from <>
  ")-[:" <> relType <> " {" <> propsToCypher props <> "}]->(" <> to <> ")"

propsToCypher :: Map Text Value -> Text
propsToCypher =
  intercalate ", " . map (\(k, v) -> k <> ": " <> valueToCypher v) . toList

valueToCypher :: Value -> Text
valueToCypher (I i) = pack $ show i
valueToCypher (F f) = pack $ show f
valueToCypher (T t) = "\"" <> t <> "\""
valueToCypher (L l) = "[" <> intercalate ", " (map valueToCypher l) <> "]"
valueToCypher _     = ""
