{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToMechanismDetails
  ( toMechanismDetails
  ) where

import           Domain.Converter.Helpers   (dropFirst, groupBySecond,
                                             groupConcat, relationOf)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Elem (SNode, SRel), Identity,
                                             exact)
import           Models                     (FOLLOW, INCLUDE, Interactant,
                                             Mechanism, MechanismDetails (..),
                                             RawMechanismDetails (..), Stage)

-- | Converts raw mechanism details to a structured `MechanismDetails` format.
-- Maps the raw data to more meaningful structures that represent mechanisms,
-- stages, and interactants.
--
-- __Parameters:__
--
-- * `RawMechanismDetails` containing the raw data of the mechanism,
-- interactants, included entities, stages, and follow-up information.
--
-- __Returns:__
--
-- * `MechanismDetails` representing the mechanism with its context
--   and associated stages and interactants.
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
