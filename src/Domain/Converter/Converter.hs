{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Converter
  ( toRawDetails
  , toReaction
  , toDetails
  ) where

import           Control.Monad         (forM)
import           Database.Bolt         (Node)
import           Domain.Converter.Type (Elem (..), Identity,
                                        Interactant (IAccelerate, ICatalyst, IMolecule, IProductFrom, IReaction, IReagentIn),
                                        NodeMask, RelMask, exact, exactRaw)
import           Models                (ACCELERATE (..), Catalyst (..),
                                        Molecule (..), PRODUCT_FROM (..),
                                        REAGENT_IN (..),
                                        RawReactionDetails (..),
                                        RawReactionDetailsMask (..),
                                        Reaction (..), ReactionDetails (..))
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
  (reaction' :: (Reaction, Identity)) <- (exact . ENode) rawReaction
  (reagents' :: [(Molecule, Identity)]) <- forM rawReagents (exact . ENode)
  (products' :: [(Molecule, Identity)]) <- forM rawProducts (exact . ENode)
  (inbound' :: [(REAGENT_IN, Identity)]) <- forM rawInbound (exact . ERel)
  (outbound' :: [(PRODUCT_FROM, Identity)]) <- forM rawOutbound (exact . ERel)
  (catalysts' :: [(Catalyst, Identity)]) <- forM rawCatalysts (exact . ENode)
  (accelerate' :: [(ACCELERATE, Identity)]) <- forM rawAccelerate (exact . ERel)
  return
    Details
      { reaction = fst reaction'
      , inboundReagents = relationOf inbound' reagents'
      , outboundProducts = relationOf outbound' products'
      , conditions = relationOf accelerate' catalysts'
      }

toRawDetails :: ReactionDetails -> IO RawReactionDetailsMask
toRawDetails Details {reaction, inboundReagents, outboundProducts, conditions} = do
  let (inbound, reagents) = unzip inboundReagents
  let (outbound, products) = unzip outboundProducts
  let (accelerate, catalysts) = unzip conditions
  (rawReactionMask :: NodeMask) <- (exactRaw . IReaction) reaction
  (rawReagentsMask :: [NodeMask]) <- forM reagents (exactRaw . IMolecule)
  (rawProductsMask :: [NodeMask]) <- forM products (exactRaw . IMolecule)
  (rawInboundMask :: [RelMask]) <- forM inbound (exactRaw . IReagentIn)
  (rawOutboundMask :: [RelMask]) <- forM outbound (exactRaw . IProductFrom)
  (rawAccelerateMask :: [RelMask]) <- forM accelerate (exactRaw . IAccelerate)
  (rawCatalystsMask :: [NodeMask]) <- forM catalysts (exactRaw . ICatalyst)
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

toReaction :: Node -> IO Reaction
toReaction rawReaction = do
  (reaction :: (Reaction, Identity)) <- (exact . ENode) rawReaction
  return $ fst reaction
