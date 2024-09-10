{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReactionDetails
  ( toReactionDetails
  ) where

import           Control.Monad         (forM)
import           Domain.Converter.Type (Subject (..), Identity, exact, relationOf)
import           Models                (ACCELERATE (..), Catalyst (..),
                                        Molecule (..), PRODUCT_FROM (..),
                                        REAGENT_IN (..),
                                        RawReactionDetails (..), Reaction (..),
                                        ReactionDetails (..))
import           Prelude               hiding (id)

toReactionDetails :: RawReactionDetails -> IO ReactionDetails
toReactionDetails RawReactionDetails { rawReaction
                     , rawReagents
                     , rawProducts
                     , rawInbound
                     , rawOutbound
                     , rawAccelerate
                     , rawCatalysts
                     } = do
  (reaction'   :: (Reaction, Identity))       <- (exact . SNode) rawReaction
  (reagents'   :: [(Molecule, Identity)])     <- forM rawReagents (exact . SNode)
  (products'   :: [(Molecule, Identity)])     <- forM rawProducts (exact . SNode)
  (inbound'    :: [(REAGENT_IN, Identity)])   <- forM rawInbound (exact . SRel)
  (outbound'   :: [(PRODUCT_FROM, Identity)]) <- forM rawOutbound (exact . SRel)
  (catalysts'  :: [(Catalyst, Identity)])     <- forM rawCatalysts (exact . SNode)
  (accelerate' :: [(ACCELERATE, Identity)])   <- forM rawAccelerate (exact . SRel)
  return
    ReactionDetails
      { reaction = fst reaction'
      , inboundReagents = relationOf inbound' reagents'
      , outboundProducts = relationOf outbound' products'
      , conditions = relationOf accelerate' catalysts'
      }
