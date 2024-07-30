{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE OverloadedLabels  #-}
-- {-# LANGUAGE TypeApplications  #-}
module Database
  ( createMolecule
  , initializeDatabase
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Default           (def)
import           Data.Map               (empty, fromList)
import           Data.Text              (pack)
import           Database.Bolt          (BoltActionT, BoltCfg, Record,
                                         Value (I, T), queryP)

-- import           Database.Bolt.Extras           (ToCypher (toCypher))
-- import           Database.Bolt.Extras.DSL       (Selector (PS), createF,
--                                                  returnF)
-- import           Database.Bolt.Extras.DSL.Typed (lbl, p, prop, (.&), (=:))
import           DataTypes              (Molecule (..))
import           Prelude                hiding (id)

defaultConfig :: BoltCfg
defaultConfig = def

initializeDatabase :: BoltActionT IO ()
initializeDatabase = do
  _ <- queryP "CREATE INDEX ON :Molecule(id)" empty
  _ <- queryP "CREATE INDEX ON :Reaction(id)" empty
  _ <- queryP "CREATE INDEX ON :Catalyst(id)" empty
  return ()

createMolecule :: MonadIO m => Molecule -> BoltActionT m [Record]
createMolecule Molecule {id, smiles, iupacName} = do
  let params =
        fromList
          [ ("id", I id)
          , ("smiles", T (pack smiles))
          , ("iupacName", T (pack iupacName))
          ]
  queryP
    "CREATE (m:Molecule {id: {id}, smiles: {smiles}, iupacName: {iupacName}})"
    params
--
-- createMolecule :: MonadIO m => Molecule -> BoltActionT m [Record]
-- createMolecule Molecule {id, smiles, iupacName} = do
--   let query = toCypher $ do
--                 createF
--                   [ PS $ p $ #m .& lbl @Molecule .& prop (#id =: id, #smiles =: pack smiles, #iupacName =: pack iupacName)
--                   ]
--                 returnF ["m"]
--   queryP query empty
