{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToRawReactionDetails
  ( toRawReactionDetails
  ) where

import           Data.Bool                  (bool)
import           Data.Default               (Default (def))
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (exactRaw)
import           Models                     (Interactant (..), NodeMask,
                                             RawReactionDetailsMask (..),
                                             ReactionDetails (..), RelMask)

def' :: Default a => [a] -> [a]
def' details = bool details [def] $ null details

-- | Converts a @ReactionDetails@ structure into a @RawReactionDetailsMask@.
-- Extracts the relevant node and relationship masks from the reaction details,
-- mapping the interactants (reagents, products, and catalysts) and reaction
-- conditions into raw representations that are suitable for database storage.
--
-- Parameters:
-- - @ReactionDetails@ - the details of the reaction, including interactants and conditions.
--
-- Returns:
-- - @RawReactionDetailsMask@ representing the raw version of reaction details
-- for database operations.
toRawReactionDetails :: ReactionDetails -> IO RawReactionDetailsMask
toRawReactionDetails ReactionDetails { reaction
                                     , inboundReagents
                                     , outboundProducts
                                     , conditions
                                     } = do
  let (inbound, reagents)           = unzip inboundReagents
  let (outbound, products)          = unzip outboundProducts
  let (accelerate, catalysts)       = unzip conditions
  (rawReactionMask   :: NodeMask)   <- (exactRaw . IReaction) reaction
  (rawReagentsMask   :: [NodeMask]) <- mapM (exactRaw . IMolecule) reagents
  (rawProductsMask   :: [NodeMask]) <- mapM (exactRaw . IMolecule) products
  (rawInboundMask    :: [RelMask])  <- mapM (exactRaw . IReagentIn) inbound
  (rawOutboundMask   :: [RelMask])  <- mapM (exactRaw . IProductFrom) outbound
  (rawAccelerateMask :: [RelMask])  <- mapM (exactRaw . IAccelerate) (def' accelerate)
  (rawCatalystsMask  :: [NodeMask]) <- mapM (exactRaw . ICatalyst) (def' catalysts)
  return
    RawReactionDetailsMask
      { rawReactionMask
      , rawReagentsMask
      , rawProductsMask
      , rawInboundMask
      , rawOutboundMask
      , rawAccelerateMask
      , rawCatalystsMask
      }
