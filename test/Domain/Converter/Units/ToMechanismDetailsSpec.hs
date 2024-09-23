{-# LANGUAGE OverloadedStrings #-}

module Domain.Converter.Units.ToMechanismDetailsSpec
  ( spec
  ) where

import           Database.Bolt              (Node (..), Relationship (..),
                                             Value (..), props)
import           Domain.Converter.Converter (toMechanismDetails)
import           Models                     (Catalyst (..), FOLLOW (..),
                                             Interactant (ICatalyst, IMolecule),
                                             Mechanism (..),
                                             MechanismDetails (..),
                                             Molecule (..),
                                             RawMechanismDetails (..),
                                             Stage (..))
import           Test.Hspec                 (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "toMechanismDetails" $ do
    it "should convert `RawMechanismDetails` to `MechanismDetails`" $ do
      let mockMechanismNode =
            Node 1 ["Mechanism"] $
            props
              [ ("id", I 4)
              , ("name", T "Electrophilic Addition")
              , ("type", T "AdE")
              , ("activationEnergy", F 75.0)
              ]
          mockInteractantNodes =
            [ Node 2 ["Molecule"] $
              props [("id", I 1), ("smiles", T "O"), ("iupacName", T "Oxidane")]
            , Node 3 ["Molecule"] $
              props
                [ ("id", I 2)
                , ("smiles", T "CCO")
                , ("iupacName", T "Ethyl alcohol")
                ]
            , Node 4 ["Molecule"] $
              props
                [("id", I 7), ("smiles", T "C=C"), ("iupacName", T "Ethene")]
            , Node 5 ["Catalyst"] $
              props
                [ ("id", I 23)
                , ("smiles", T "O=P(O)(O)O")
                , ("name", T "Phosphoric Acid")
                ]
            , Node 6 ["Catalyst"] $
              props
                [ ("id", I 24)
                , ("smiles", T "O=S(=O)(O)O")
                , ("name", T "Sulfuric Acid")
                ]
            , Node 7 ["Catalyst"] $
              props
                [ ("id", I 25)
                , ("smiles", T "O=[Si]O[Si](O)O[Si](O)O")
                , ("name", T "Zeolite")
                ]
            ]
          mockIncludeRel =
            [ Relationship 1 4 8 "INCLUDE" $ props []
            , Relationship 2 5 8 "INCLUDE" $ props []
            , Relationship 3 6 8 "INCLUDE" $ props []
            , Relationship 4 7 8 "INCLUDE" $ props []
            , Relationship 5 2 9 "INCLUDE" $ props []
            , Relationship 6 3 9 "INCLUDE" $ props []
            , Relationship 7 3 10 "INCLUDE" $ props []
            ]
          mockStageNodes =
            [ Node 8 ["Stage"] $
              props
                [ ("order", I 1)
                , ("name", T "Protonation of ethene")
                , ( "description"
                  , T "Ethene is protonated, forming a carbocation")
                , ("products", L [T "Carbocation"])
                ]
            , Node 9 ["Stage"] $
              props
                [ ("order", I 2)
                , ("name", T "Nucleophilic attack by water")
                , ("description", T "Water molecule attacks the carbocation")
                , ("products", L [T "Ethanolium"])
                ]
            , Node 10 ["Stage"] $
              props
                [ ("order", I 3)
                , ("name", T "Deprotonation")
                , ("description", T "Excess proton is removed, forming ethanol")
                , ("products", L [T "Ethanol"])
                ]
            ]
          mockFollowRel =
            Relationship 1 2 1 "FOLLOW" $
            props
              [("description", T "Electrophilic addition of water to ethene")]
      let mockRawMechanismDetails =
            RawMechanismDetails
              { rawMechanism = mockMechanismNode
              , rawInteractants = mockInteractantNodes
              , rawInclude = mockIncludeRel
              , rawStages = mockStageNodes
              , rawFollow = mockFollowRel
              }
      let mockMechanism =
            Mechanism
              { mechanismId = 4
              , mechanismName = "Electrophilic Addition"
              , mechanismType = "AdE"
              , mechanismActivationEnergy = 75.0
              }
          mockFollow =
            FOLLOW {description = "Electrophilic addition of water to ethene"}
          mockStage1 =
            Stage
              { stageOrder = 1
              , stageName = "Protonation of ethene"
              , stageDescription = "Ethene is protonated, forming a carbocation"
              , stageProducts = ["Carbocation"]
              }
          mockStage2 =
            Stage
              { stageOrder = 2
              , stageName = "Nucleophilic attack by water"
              , stageDescription = "Water molecule attacks the carbocation"
              , stageProducts = ["Ethanolium"]
              }
          mockStage3 =
            Stage
              { stageOrder = 3
              , stageName = "Deprotonation"
              , stageDescription = "Excess proton is removed, forming ethanol"
              , stageProducts = ["Ethanol"]
              }
          mockInteractants1 =
            [ IMolecule
                Molecule
                  { moleculeId = 7
                  , moleculeSmiles = "C=C"
                  , moleculeIupacName = "Ethene"
                  }
            , ICatalyst
                Catalyst
                  { catalystId = 23
                  , catalystSmiles = "O=P(O)(O)O"
                  , catalystName = Just "Phosphoric Acid"
                  }
            , ICatalyst
                Catalyst
                  { catalystId = 24
                  , catalystSmiles = "O=S(=O)(O)O"
                  , catalystName = Just "Sulfuric Acid"
                  }
            , ICatalyst
                Catalyst
                  { catalystId = 25
                  , catalystSmiles = "O=[Si]O[Si](O)O[Si](O)O"
                  , catalystName = Just "Zeolite"
                  }
            ]
          mockInteractants2 =
            [ IMolecule
                Molecule
                  { moleculeId = 1
                  , moleculeSmiles = "O"
                  , moleculeIupacName = "Oxidane"
                  }
            , IMolecule
                Molecule
                  { moleculeId = 2
                  , moleculeSmiles = "CCO"
                  , moleculeIupacName = "Ethyl alcohol"
                  }
            ]
          mockInteractants3 =
            [ IMolecule
                Molecule
                  { moleculeId = 2
                  , moleculeSmiles = "CCO"
                  , moleculeIupacName = "Ethyl alcohol"
                  }
            ]
      let expectedMechanismDetails =
            MechanismDetails
              { mechanismContext = (mockMechanism, mockFollow)
              , stageInteractants =
                  [ (mockStage1, mockInteractants1)
                  , (mockStage2, mockInteractants2)
                  , (mockStage3, mockInteractants3)
                  ]
              }
      result <- toMechanismDetails mockRawMechanismDetails
      result `shouldBe` expectedMechanismDetails
