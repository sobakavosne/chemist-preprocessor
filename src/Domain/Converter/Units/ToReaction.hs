{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReaction
  ( toReaction
  ) where

import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Elem (SNode), Identity, exact)
import           Models                     (Reaction (..), ReactionNode)
import           Prelude                    hiding (id)

-- | Converts a raw `ReactionNode` into a `Reaction` structure.
--
-- __Parameters:__
--
-- * `ReactionNode` - the raw reaction node retrieved from the database.
--
-- __Returns:__
--
-- * `Reaction` representing the processed reaction node, with only the reaction data.
toReaction :: ReactionNode -> IO Reaction
toReaction rawReaction = do
  (reaction :: (Reaction, Identity)) <- (exact . SNode) rawReaction
  return $ fst reaction
