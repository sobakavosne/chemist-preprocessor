{-# LANGUAGE OverloadedStrings #-}

module Domain.Converter.Units.ToRawDetailsSpec
  ( toRawDetailsSpec
  ) where

import           Data.Map                            (fromList)
import           Database.Bolt                       (Value (..), props)
import           Domain.Converter.Units.ToRawDetails (toRawDetails)
import           Models                              (ACCELERATE (..),
                                                      Catalyst (..),
                                                      Molecule (..),
                                                      NodeMask (..),
                                                      PRODUCT_FROM (..),
                                                      REAGENT_IN (..),
                                                      RawReactionDetailsMask (..),
                                                      Reaction (..),
                                                      ReactionDetails (..),
                                                      RelMask (..))
import           Test.Hspec                          (Spec, describe, it,
                                                      shouldBe)

testReaction :: Reaction
testReaction = Reaction {reactionId = 1, reactionName = "Test Reaction"}

testMolecule :: Molecule
testMolecule =
  Molecule {moleculeId = 1, moleculeSmiles = "C", moleculeIupacName = "Methane"}

testCatalyst :: Catalyst
testCatalyst =
  Catalyst
    {catalystId = 1, catalystSmiles = "C", catalystName = "Test Catalyst"}

testAccelerate :: ACCELERATE
testAccelerate = ACCELERATE {temperature = [273.15], pressure = [101.325]}

testReagentIn :: REAGENT_IN
testReagentIn = REAGENT_IN {reagentAmount = 1.0}

testProductFrom :: PRODUCT_FROM
testProductFrom = PRODUCT_FROM {productAmount = 2.0}

testReactionDetails :: ReactionDetails
testReactionDetails =
  ReactionDetails
    { reaction = testReaction
    , inboundReagents = [(testReagentIn, testMolecule)]
    , outboundProducts = [(testProductFrom, testMolecule)]
    , conditions = [(testAccelerate, testCatalyst)]
    }

toRawDetailsSpec :: Spec
toRawDetailsSpec = do
  describe "toRawDetails" $ do
    it "should convert `ReactionDetails` to `RawReactionDetailsMask`" $ do
      let expectedRawReactionMask =
            NodeMask
              {nodePropsMask = props [("id", I 1), ("name", T "Test Reaction")]}
      let expectedRawReagentsMask =
            [ NodeMask
                { nodePropsMask =
                    props
                      [ ("id", I 1)
                      , ("smiles", T "C")
                      , ("iupacName", T "Methane")
                      ]
                }
            ]
      let expectedRawProductsMask =
            [ NodeMask
                { nodePropsMask =
                    props
                      [ ("id", I 1)
                      , ("smiles", T "C")
                      , ("iupacName", T "Methane")
                      ]
                }
            ]
      let expectedRawInboundMask =
            [RelMask {relPropsMask = props [("amount", F 1.0)]}]
      let expectedRawOutboundMask =
            [RelMask {relPropsMask = props [("amount", F 2.0)]}]
      let expectedRawAccelerateMask =
            [ RelMask
                { relPropsMask =
                    props
                    -- Oh!
                      [ ("pressure", L [F 101.32499694824219])
                      , ("temperature", L [F 273.1499938964844])
                      ]
                }
            ]
      let expectedRawCatalystsMask =
            [ NodeMask
                { nodePropsMask =
                    fromList
                      [ ("id", I 1)
                      , ("smiles", T "C")
                      , ("name", T "Test Catalyst")
                      ]
                }
            ]
      result <- toRawDetails testReactionDetails
      let expectedResult =
            RawDetailsMask
              { rawReactionMask = expectedRawReactionMask
              , rawReagentsMask = expectedRawReagentsMask
              , rawProductsMask = expectedRawProductsMask
              , rawInboundMask = expectedRawInboundMask
              , rawOutboundMask = expectedRawOutboundMask
              , rawAccelerateMask = expectedRawAccelerateMask
              , rawCatalystsMask = expectedRawCatalystsMask
              }
      result `shouldBe` expectedResult
