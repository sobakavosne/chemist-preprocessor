{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReaction
  ( toReaction
  ) where

import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Identity, Subject (SNode), exact)
import           Models                     (Reaction (..), ReactionNode)
import           Prelude                    hiding (id)

toReaction :: ReactionNode -> IO Reaction
toReaction rawReaction = do
  (reaction :: (Reaction, Identity)) <- (exact . SNode) rawReaction
  return $ fst reaction
