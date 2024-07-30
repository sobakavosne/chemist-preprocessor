{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initializeDatabase
  ) where

import           Data.Map               (empty)
import           Data.Text              (pack)
import           Database.Bolt          (BoltActionT, queryP)

import           DataTypes              (Molecule (..))
import           Prelude                hiding (id)

initializeDatabase :: BoltActionT IO ()
initializeDatabase = do
  _ <- queryP "CREATE INDEX ON :Molecule(id)" empty
  _ <- queryP "CREATE INDEX ON :Reaction(id)" empty
  _ <- queryP "CREATE INDEX ON :Catalyst(id)" empty
  return ()
