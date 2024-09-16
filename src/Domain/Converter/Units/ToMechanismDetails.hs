{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToMechanismDetails
  ( toMechanismDetails
  ) where

import           Domain.Converter.Helpers   (dropFirst, groupBySecond,
                                             groupConcat, relationOf)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Identity, Subject (SNode, SRel),
                                             exact)
import           Models                     (FOLLOW, INCLUDE, Interactant,
                                             Mechanism, MechanismDetails (..),
                                             RawMechanismDetails (..), Stage)

toMechanismDetails :: RawMechanismDetails -> IO MechanismDetails
toMechanismDetails RawMechanismDetails { rawMechanism
                                       , rawInteractants
                                       , rawInclude
                                       , rawStages
                                       , rawFollow
                                       } = do
  (mechanism    :: (Mechanism, Identity))           <- (exact . SNode) rawMechanism
  (follow       :: (FOLLOW, Identity))              <- (exact . SRel) rawFollow
  (interactants :: [(Interactant, Identity)])       <- mapM (exact . SNode) rawInteractants
  (include      :: [(INCLUDE, Identity, Identity)]) <- mapM (exact . SRel) rawInclude
  (stages       :: [(Stage, Identity)])             <- mapM (exact . SNode) rawStages
  return
    MechanismDetails
      { mechanismContext = head $ relationOf [mechanism] [follow]
      , stageInteractants =
          groupConcat $
          stages `relationOf` map dropFirst include `relationOf`
          groupBySecond interactants
      }
