module Infrastructure.Database
  ( withNeo4j
  , checkNeo4j
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (pack)
import           Database.Bolt          (BoltActionT, BoltCfg, connect, query,
                                         run)

withNeo4j :: MonadIO m => BoltCfg -> BoltActionT m b -> m b
withNeo4j cfg action = do
  pipe <- connect cfg
  run pipe action

checkNeo4j :: BoltActionT IO Bool
checkNeo4j = do
  result <- query (pack "RETURN 1")
  return $
    case result of
      [] -> False
      _  -> True
-- initializeDatabase :: BoltActionT IO ()
-- initializeDatabase = do
--   _ <- queryP "CREATE INDEX ON :Molecule(id)" empty
--   _ <- queryP "CREATE INDEX ON :Reaction(id)" empty
--   _ <- queryP "CREATE INDEX ON :Catalyst(id)" empty
--   return ()
-- createMolecule :: MonadIO m => Molecule -> BoltActionT m [Record]
-- createMolecule Molecule {id, smiles, iupacName} = do
--   let params =
--         fromList
--           [ ("id", I id)
--           , ("smiles", T (pack smiles))
--           , ("iupacName", T (pack iupacName))
--           ]
--   queryP
--     "CREATE (m:Molecule {id: {id}, smiles: {smiles}, iupacName: {iupacName}})"
--     params
--
-- createMolecule :: MonadIO m => Molecule -> BoltActionT m [Record]
-- createMolecule Molecule {id, smiles, iupacName} = do
--   let query = toCypher $ do
--                 createF
--                   [ PS $ p $ #m .& lbl @Molecule .& prop (#id =: id, #smiles =: pack smiles, #iupacName =: pack iupacName)
--                   ]
--                 returnF ["m"]
--   queryP query empty
