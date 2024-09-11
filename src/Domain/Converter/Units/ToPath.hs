{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToPath
  ( toPath
  ) where

import           Database.Bolt              (Path)
import           Domain.Converter.Instances ()
import           Domain.Converter.Type      (Subject (SPath), exact)
import           Models                     (PathMask)

toPath :: Path -> IO PathMask
toPath rawPath = do
  (pathMask :: PathMask) <- (exact . SPath) rawPath
  return pathMask
