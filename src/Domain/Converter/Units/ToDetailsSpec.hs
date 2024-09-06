{-# LANGUAGE OverloadedStrings #-}

module Domain.Converter.Units.ToDetailsSpec
  ( toDetailsSpec
  ) where

import           Database.Bolt                    (Node (..), Relationship (..),
                                                   Value (F, I, L, T), props)
import           Domain.Converter.Units.ToDetails (toDetails)
import           Models                           (ACCELERATE (..),
                                                   Catalyst (..), Molecule (..),
                                                   PRODUCT_FROM (..),
                                                   REAGENT_IN (..),
                                                   RawReactionDetails (..),
                                                   Reaction (..),
                                                   ReactionDetails (..))
import           Test.Hspec                       (Spec, describe, it, shouldBe)

toDetailsSpec :: Spec
toDetailsSpec = do
  describe "toDetails" $ do
    it "should convert `RawReactionDetails` to `ReactionDetails`" $ do
      let mockReagentNode =
            Node 1 ["Molecule"] $
            props [("id", I 1), ("smiles", T "CO"), ("iupacName", T "Methanol")]
      let mockProductNode =
            Node 3 ["Molecule"] $
            props [("id", I 2), ("smiles", T "CO"), ("iupacName", T "Methanol")]
      let mockReactionNode =
            Node 2 ["Reaction"] $
            props [("id", I 1), ("name", T "Sample Reaction")]
      let mockCatalystNode =
            Node 4 ["Catalyst"] $
            props [("id", I 1), ("smiles", T "H2O"), ("name", T "Water")]
      let mockInboundRel =
            Relationship 1 1 2 "REAGENT_IN" $ props [("amount", F 2.0)]
      let mockProductRel =
            Relationship 2 2 3 "PRODUCT_FROM" $ props [("amount", F 1.0)]
      let mockAccelerateRel =
            Relationship 3 4 2 "ACCELERATE" $
            props [("temperature", L [F 273.15]), ("pressure", L [F 101.325])]
      let mockRawReactionDetails =
            RawDetails
              { rawReaction = mockReactionNode
              , rawReagents = [mockReagentNode]
              , rawProducts = [mockProductNode]
              , rawInbound = [mockInboundRel]
              , rawOutbound = [mockProductRel]
              , rawAccelerate = [mockAccelerateRel]
              , rawCatalysts = [mockCatalystNode]
              }
      let expectedReactionDetails =
            Details
              { reaction =
                  Reaction {reactionId = 1, reactionName = "Sample Reaction"}
              , inboundReagents = [(REAGENT_IN 2.0, Molecule 1 "CO" "Methanol")]
              , outboundProducts =
                  [(PRODUCT_FROM 1.0, Molecule 2 "CO" "Methanol")]
              , conditions =
                  [(ACCELERATE [273.15] [101.325], Catalyst 1 "H2O" "Water")]
              }
      result <- toDetails mockRawReactionDetails
      result `shouldBe` expectedReactionDetails
