{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToPath
  ( toPath
  ) where

import           Database.Bolt              (Path)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Elem (SPath), exact)
import           Models                     (PathMask)

-- | Converts a raw database `Path` into a `PathMask`.
--
-- __Parameters:__
--
-- * `Path` - the raw path data retrieved from the database.
--
-- __Returns:__
--
-- * `PathMask` representing the processed path in a usable format.
toPath :: Path -> IO PathMask
toPath rawPath = do
  (pathMask :: PathMask) <- (exact . SPath) rawPath
  return pathMask
