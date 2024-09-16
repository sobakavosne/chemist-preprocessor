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
