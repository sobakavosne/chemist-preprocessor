module Domain.Helpers
  ( fromDouble
  ) where

fromDouble :: Double -> Float
fromDouble = realToFrac :: Double -> Float
