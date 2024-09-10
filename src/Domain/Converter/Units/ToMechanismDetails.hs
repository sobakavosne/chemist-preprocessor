{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToMechanismDetails
  ( toMechanismDetails
  ) where

import           Data.Function         (on)
import           Data.List             (groupBy)
import           Domain.Converter.Type (Identity, Interactant,
                                        Subject (SNode, SRel), dropFirst, exact,
                                        groupBySecond, relationOf)
import           Models                (FOLLOW, INCLUDE, Mechanism,
                                        MechanismDetails (..),
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

groupConcat :: Eq a => [(a, [b])] -> [(a, [b])]
groupConcat xs = map combine $ groupBy ((==) `on` fst) xs
  where
    combine grp = (fst (head grp), concatMap snd grp)
