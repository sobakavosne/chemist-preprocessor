{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToRawDetails
  ( toRawDetails
  ) where

import           Control.Monad              (forM)
import           Data.Bool                  (bool)
import           Data.Default               (Default (def))
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (exactRaw)
import           Models                     (Interactant (..), NodeMask,
                                             RawReactionDetailsMask (..),
                                             ReactionDetails (..), RelMask)

def' :: Default a => [a] -> [a]
def' details = bool details [def] $ null details

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
  (rawAccelerateMask :: [RelMask])  <- forM (def' accelerate) (exactRaw . IAccelerate)
  (rawCatalystsMask  :: [NodeMask]) <- forM (def' catalysts) (exactRaw . ICatalyst)
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
