module Infrastructure.Type
  ( GraphElemError(..)
  ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)

newtype GraphElemError =
  GraphElemError Text
  deriving (Eq, Show)

instance Exception GraphElemError
