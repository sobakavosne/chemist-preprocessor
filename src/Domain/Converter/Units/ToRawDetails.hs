{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToRawDetails
  ( toRawDetails
  ) where

import           Control.Monad         (forM)
import           Domain.Converter.Type (Interactant (IAccelerate, ICatalyst, IMolecule, IProductFrom, IReaction, IReagentIn),
                                        exactRaw)
import           Models                (NodeMask, RawReactionDetailsMask (..),
                                        ReactionDetails (..), RelMask)

toRawDetails :: ReactionDetails -> IO RawReactionDetailsMask
toRawDetails ReactionDetails { reaction
                             , inboundReagents
                             , outboundProducts
                             , conditions
                             } = do
  let (inbound, reagents)           = unzip inboundReagents
  let (outbound, products)          = unzip outboundProducts
  let (accelerate, catalysts)       = unzip conditions
  (rawReactionMask   :: NodeMask)   <- (exactRaw . IReaction) reaction
  (rawReagentsMask   :: [NodeMask]) <- forM reagents (exactRaw . IMolecule)
  (rawProductsMask   :: [NodeMask]) <- forM products (exactRaw . IMolecule)
  (rawInboundMask    :: [RelMask])  <- forM inbound (exactRaw . IReagentIn)
  (rawOutboundMask   :: [RelMask])  <- forM outbound (exactRaw . IProductFrom)
  (rawAccelerateMask :: [RelMask])  <- forM accelerate (exactRaw . IAccelerate)
  (rawCatalystsMask  :: [NodeMask]) <- forM catalysts (exactRaw . ICatalyst)
  return
    RawDetailsMask
      { rawReactionMask
      , rawReagentsMask
      , rawProductsMask
      , rawInboundMask
      , rawOutboundMask
      , rawAccelerateMask
      , rawCatalystsMask
      }
