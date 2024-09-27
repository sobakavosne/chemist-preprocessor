module Domain.Converter.Helpers
  ( groupBySecond
  , groupConcat
  , relationOf
  , dropFirst
  ) where

import           Data.Function   (on)
import           Data.List       (groupBy)
import           Data.Map.Strict (empty, insertWith, toList)
import           Data.Tuple      (swap)

-- | Let's suppose we have a unique values
relationOf :: Eq b => [(a, b)] -> [(c, b)] -> [(a, c)]
relationOf xs ys = [(x1, y1) | (x1, x2) <- xs, (y1, y2) <- ys, x2 == y2]

dropFirst :: (a, b, c) -> (b, c)
dropFirst (_, b, c) = (b, c)

groupBySecond :: (Ord b) => [(a, b)] -> [([a], b)]
groupBySecond = map swap . toList . foldr insertInMap empty
  where
    insertInMap (a, b) = insertWith (++) b [a]

groupConcat :: Eq a => [(a, [b])] -> [(a, [b])]
groupConcat = map combine . groupBy ((==) `on` fst)
  where
    combine group = (fst (head group), concatMap snd group)
