module Main
  ( main
  ) where

import           Domain.Converter.Units.ToMechanismDetailsSpec (toMechanismDetailsSpec)
import           Domain.Converter.Units.ToPathSpec             (toPathSpec)
import           Domain.Converter.Units.ToRawDetailsSpec       (toRawDetailsSpec)
import           Domain.Converter.Units.ToReactionDetailsSpec  (toReactionDetailsSpec)
import           Domain.Converter.Units.ToReactionSpec         (toReactionSpec)
import           Test.Hspec                                    (describe, hspec)

main :: IO ()
main =
  hspec $ do
    describe "Domain.Converter.Units.ToMechanismDetails" toMechanismDetailsSpec
    describe "Domain.Converter.Units.ToPath" toPathSpec
    describe "Domain.Converter.Units.ToRawDetails" toRawDetailsSpec
    describe "Domain.Converter.Units.ToReactionDetails" toReactionDetailsSpec
    describe "Domain.Converter.Units.ToReaction" toReactionSpec
