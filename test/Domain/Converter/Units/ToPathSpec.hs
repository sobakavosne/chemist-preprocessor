{-# LANGUAGE OverloadedStrings #-}

module Domain.Converter.Units.ToPathSpec
  ( spec
  ) where

import           Database.Bolt                 (Node (Node), Path (Path),
                                                URelationship (URelationship),
                                                Value (F, I, T), props)
import           Domain.Converter.Units.ToPath (toPath)
import           Models                        (Interactant (..), Molecule (..),
                                                PathMask (..),
                                                REAGENT_IN (REAGENT_IN))
import           Test.Hspec                    (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "toPath" $ do
    it "should convert Bolt `Path` to a `PathMask`" $ do
      let mockNode1 =
            Node
              1
              ["Molecule"]
              (props
                 [("id", I 1), ("smiles", T "C"), ("iupacName", T "Methane")])
      let mockNode2 =
            Node
              2
              ["Molecule"]
              (props [("id", I 2), ("smiles", T "O"), ("iupacName", T "Oxygen")])
      let mockRel1 = URelationship 1 "REAGENT_IN" (props [("amount", F 2.5)])
      let mockPath = Path [mockNode1, mockNode2] [mockRel1] [0, 1, 1, 2]
      result <- toPath mockPath
      let expectedResult =
            PathMask
              { pathNodesMask =
                  [ IMolecule
                      Molecule
                        { moleculeId = 1
                        , moleculeSmiles = "C"
                        , moleculeIupacName = "Methane"
                        }
                  , IMolecule
                      Molecule
                        { moleculeId = 2
                        , moleculeSmiles = "O"
                        , moleculeIupacName = "Oxygen"
                        }
                  ]
              , pathRelationshipsMask = [IReagentIn (REAGENT_IN 2.5)]
              , pathSequenceMask = [0, 1, 1, 2]
              }
      result `shouldBe` expectedResult
