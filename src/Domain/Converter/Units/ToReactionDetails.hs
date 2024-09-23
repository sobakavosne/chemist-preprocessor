{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReactionDetails
  ( toReactionDetails
  ) where

import           Control.Monad              (forM)
import           Domain.Converter.Helpers   (relationOf)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Identity, Subject (SNode, SRel),
                                             exact)
import           Models                     (ACCELERATE (..), Catalyst (..),
                                             Mechanism (mechanismId),
                                             MechanismID, MechanismNode,
                                             Molecule (..), PRODUCT_FROM (..),
                                             REAGENT_IN (..),
                                             RawReactionDetails (..),
                                             Reaction (..),
                                             ReactionDetails (..))

-- | Converts raw reaction details retrieved from the database into a structured
--   format of @ReactionDetails@.
--   This function processes the reaction, reagents, products, inbound and outbound
--   relationships, catalysts, and acceleration factors, and also processes an optional
--   mechanism if one is associated with the reaction.
--
-- Parameters:
-- - @(RawReactionDetails, Maybe MechanismNode)@ - The raw reaction details and optional mechanism node.
--
-- Returns:
-- - A tuple of @ReactionDetails@ and an optional @MechanismID@, representing the
--   reaction's details and an associated mechanism ID, if applicable.
toReactionDetails ::
     (RawReactionDetails, Maybe MechanismNode)
  -> IO (ReactionDetails, Maybe MechanismID)
toReactionDetails (RawReactionDetails { rawReaction
                                      , rawReagents
                                      , rawProducts
                                      , rawInbound
                                      , rawOutbound
                                      , rawAccelerate
                                      , rawCatalysts
                                      }, rawMechanism) = do
  (reaction   :: (Reaction, Identity))        <- (exact . SNode) rawReaction
  (reagents   :: [(Molecule, Identity)])      <- forM rawReagents (exact . SNode)
  (products   :: [(Molecule, Identity)])      <- forM rawProducts (exact . SNode)
  (inbound    :: [(REAGENT_IN, Identity)])    <- forM rawInbound (exact . SRel)
  (outbound   :: [(PRODUCT_FROM, Identity)])  <- forM rawOutbound (exact . SRel)
  (catalysts  :: [(Catalyst, Identity)])      <- forM rawCatalysts (exact . SNode)
  (accelerate :: [(ACCELERATE, Identity)])    <- forM rawAccelerate (exact . SRel)
  (mechanism  :: Maybe (Mechanism, Identity)) <- traverse (exact . SNode) rawMechanism
  return
    ( ReactionDetails
        { reaction = fst reaction
        , inboundReagents = relationOf inbound reagents
        , outboundProducts = relationOf outbound products
        , conditions = relationOf accelerate catalysts
        }
    , mechanismId . fst <$> mechanism)
