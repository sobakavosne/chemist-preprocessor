{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Converter.Units.ToPath
  ( toPath
  ) where

import           Database.Bolt         (Path)
import           Domain.Converter.Type (Elem (EPath), exact)
import           Models                (PathMask)

toPath :: Path -> IO PathMask
toPath rawPath = do
  (pathMask :: PathMask) <- (exact . EPath) rawPath
  return pathMask
