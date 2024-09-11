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
                                             RawMechanismDetails (..),
                                             Reaction (reactionId), ReactionId,
                                             ReactionNode, Stage)

toMechanismDetails ::
     (RawMechanismDetails, ReactionNode) -> IO (MechanismDetails, ReactionId)
toMechanismDetails (RawMechanismDetails { rawMechanism
                                        , rawInteractants
                                        , rawInclude
                                        , rawStages
                                        , rawFollow
                                        }, rawReaction) = do
  (mechanism    :: (Mechanism, Identity))           <- (exact . SNode) rawMechanism
  (follow       :: (FOLLOW, Identity))              <- (exact . SRel) rawFollow
  (interactants :: [(Interactant, Identity)])       <- mapM (exact . SNode) rawInteractants
  (include      :: [(INCLUDE, Identity, Identity)]) <- mapM (exact . SRel) rawInclude
  (stages       :: [(Stage, Identity)])             <- mapM (exact . SNode) rawStages
  (reaction     :: (Reaction, Identity))            <- (exact . SNode) rawReaction
  return
    ( MechanismDetails
        { mechanismContext = head $ relationOf [mechanism] [follow]
        , stageInteractants =
            groupConcat $
            stages `relationOf` map dropFirst include `relationOf`
            groupBySecond interactants
        }
    , (reactionId . fst) reaction)
