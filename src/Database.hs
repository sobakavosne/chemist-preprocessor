{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initializeDatabase
  ) where

import           Data.Map      (empty)

-- import           Data.Text          (Text)
import           Database.Bolt (BoltActionT, RecordValue (..), at, queryP)

-- import           Database.Bolt.Lazy (Node)
-- import           DataTypes          (Catalyst (..), Molecule (..),
--                                      Reaction (..))
initializeDatabase :: BoltActionT IO ()
initializeDatabase = do
  queryP "CREATE INDEX ON :Molecule(id)" empty
  queryP "CREATE INDEX ON :Reaction(id)" empty
  queryP "CREATE INDEX ON :Catalyst(id)" empty
  return ()
