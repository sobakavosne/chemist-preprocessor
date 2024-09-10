{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReaction
  ( toReaction
  ) where

import           Database.Bolt         (Node)
import           Domain.Converter.Type (Subject (..), Identity, exact)
import           Models                (Reaction (..))
import           Prelude               hiding (id)

toReaction :: Node -> IO Reaction
toReaction rawReaction = do
  (reaction :: (Reaction, Identity)) <- (exact . SNode) rawReaction
  return $ fst reaction
