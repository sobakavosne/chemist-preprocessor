{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToDetails
  ( toDetails
  ) where

import           Control.Monad         (forM)
import           Domain.Converter.Type (Elem (..), Identity, exact)
import           Models                (ACCELERATE (..), Catalyst (..),
                                        Molecule (..), PRODUCT_FROM (..),
                                        REAGENT_IN (..),
                                        RawReactionDetails (..), Reaction (..),
                                        ReactionDetails (..))
import           Prelude               hiding (id)

-- Let's suppose we have a unique values
relationOf :: Eq b => [(a, b)] -> [(c, b)] -> [(a, c)]
relationOf xs ys = [(x1, y1) | (x1, x2) <- xs, (y1, y2) <- ys, x2 == y2]

toDetails :: RawReactionDetails -> IO ReactionDetails
toDetails RawDetails { rawReaction
                     , rawReagents
                     , rawProducts
                     , rawInbound
                     , rawOutbound
                     , rawAccelerate
                     , rawCatalysts
                     } = do
  (reaction'   :: (Reaction, Identity))       <- (exact . ENode) rawReaction
  (reagents'   :: [(Molecule, Identity)])     <- forM rawReagents (exact . ENode)
  (products'   :: [(Molecule, Identity)])     <- forM rawProducts (exact . ENode)
  (inbound'    :: [(REAGENT_IN, Identity)])   <- forM rawInbound (exact . ERel)
  (outbound'   :: [(PRODUCT_FROM, Identity)]) <- forM rawOutbound (exact . ERel)
  (catalysts'  :: [(Catalyst, Identity)])     <- forM rawCatalysts (exact . ENode)
  (accelerate' :: [(ACCELERATE, Identity)])   <- forM rawAccelerate (exact . ERel)
  return
    Details
      { reaction = fst reaction'
      , inboundReagents = relationOf inbound' reagents'
      , outboundProducts = relationOf outbound' products'
      , conditions = relationOf accelerate' catalysts'
      }
