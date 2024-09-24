{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReactionDetails
  ( toReactionDetails
  ) where

import           Domain.Converter.Helpers   (relationOf)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Elem (SNode, SRel), Identity,
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
  (reagents   :: [(Molecule, Identity)])      <- mapM (exact . SNode) rawReagents
  (products   :: [(Molecule, Identity)])      <- mapM (exact . SNode) rawProducts
  (inbound    :: [(REAGENT_IN, Identity)])    <- mapM (exact . SRel) rawInbound
  (outbound   :: [(PRODUCT_FROM, Identity)])  <- mapM (exact . SRel) rawOutbound
  (catalysts  :: [(Catalyst, Identity)])      <- mapM (exact . SNode) rawCatalysts
  (accelerate :: [(ACCELERATE, Identity)])    <- mapM  (exact . SRel) rawAccelerate
  (mechanism  :: Maybe (Mechanism, Identity)) <- traverse (exact . SNode) rawMechanism
  return
    ( ReactionDetails
        { reaction = fst reaction
        , inboundReagents = relationOf inbound reagents
        , outboundProducts = relationOf outbound products
        , conditions = relationOf accelerate catalysts
        }
    , mechanismId . fst <$> mechanism)
