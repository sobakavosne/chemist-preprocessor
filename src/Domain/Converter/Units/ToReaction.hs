{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToReaction
  ( toReaction
  ) where

import           Database.Bolt              (Node)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Identity, Subject (SNode), exact)
import           Models                     (Reaction (..))
import           Prelude                    hiding (id)

toReaction :: Node -> IO Reaction
toReaction rawReaction = do
  (reaction :: (Reaction, Identity)) <- (exact . SNode) rawReaction
  return $ fst reaction
