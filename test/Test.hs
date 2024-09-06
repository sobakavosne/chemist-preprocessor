module Main
  ( main
  ) where

import           Domain.Converter.Units.ToDetailsSpec    (toDetailsSpec)
import           Domain.Converter.Units.ToPathSpec       (toPathSpec)
import           Domain.Converter.Units.ToRawDetailsSpec (toRawDetailsSpec)
import           Domain.Converter.Units.ToReactionSpec   (toReactionSpec)
import           Test.Hspec                              (describe, hspec)

main :: IO ()
main =
  hspec $ do
    describe "Domain.Converter.Units.ToDetails" toDetailsSpec
    describe "Domain.Converter.Units.ToPath" toPathSpec
    describe "Domain.Converter.Units.ToRawDetails" toRawDetailsSpec
    describe "Domain.Converter.Units.ToReaction" toReactionSpec
