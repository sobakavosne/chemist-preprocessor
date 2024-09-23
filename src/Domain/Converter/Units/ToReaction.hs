{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReaction
  ( toReaction
  ) where

import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Identity, Subject (SNode), exact)
import           Models                     (Reaction (..), ReactionNode)
import           Prelude                    hiding (id)

-- | Converts a raw @ReactionNode@ into a @Reaction@ structure.
--
-- Parameters:
-- - @ReactionNode@ - the raw reaction node retrieved from the database.
--
-- Returns:
-- - @Reaction@ representing the processed reaction node, with only the reaction data.
toReaction :: ReactionNode -> IO Reaction
toReaction rawReaction = do
  (reaction :: (Reaction, Identity)) <- (exact . SNode) rawReaction
  return $ fst reaction
