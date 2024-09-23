{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Converter.Units.ToReactionSpec
  ( spec
  ) where

import           Database.Bolt                     (Node (..), Value (..),
                                                    props)
import           Domain.Converter.Units.ToReaction (toReaction)
import           Models                            (Reaction (..))
import           Test.Hspec                        (Spec, describe, it,
                                                    shouldBe)

spec :: Spec
spec = do
  describe "toReaction" $ do
    it "should convert a valid Node to a Reaction" $ do
      let nodeIdentity = 1
          labels = ["Reaction"]
          nodeProps = props [("id", I 10), ("name", T "Test Reaction")]
          node = Node {nodeIdentity, labels, nodeProps}
      let expectedReaction =
            Reaction {reactionId = 10, reactionName = "Test Reaction"}
      reaction <- toReaction node
      reaction `shouldBe` expectedReaction
