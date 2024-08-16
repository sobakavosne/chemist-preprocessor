{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Database
  ( withNeo4j
  , checkNeo4j
  , getReaction
  ) where

import           Control.Monad.Except   (MonadError (catchError))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (pack)
import           Database.Bolt          (BoltActionT, BoltCfg, BoltError,
                                         Value (I), connect, props, query,
                                         queryP, run)
import           Helpers                (unrecord)
import           Models                 (ACCELERATE, Catalyst, Molecule,
                                         PRODUCT_FROM, REAGENT_IN,
                                         Reaction (..), ReactionDetails (..))
import           Prelude                hiding (id)

withNeo4j :: MonadIO m => BoltCfg -> BoltActionT m b -> m b
withNeo4j cfg action = do
  pipe <- connect cfg
  run pipe action

checkNeo4j :: BoltActionT IO Bool
checkNeo4j = do
  result <- query (pack "RETURN 1") `catchError` handleException
  return $ not (null result)
  where
    handleException :: BoltError -> BoltActionT IO [a]
    handleException _ = return []

getReaction :: Int -> BoltActionT IO ReactionDetails
getReaction id = do
  result <-
    queryP
      "MATCH \
             \(reaction:Reaction { id: $id })<-[accelerate:ACCELERATE]-(catalyst:Catalyst), \
             \(reaction)-[product_from:PRODUCT_FROM]->(product:Molecule), \
             \(reaction)<-[reagent_in:REAGENT_IN]-(reagent:Molecule) \
             \RETURN reaction, accelerate, catalyst, product, reagent, product_from, reagent_in"
      (props [("id", I id)])
  (reaction :: Reaction) <- head <$> unrecord result "reaction"
  (reagents :: [Molecule]) <- unrecord result "reagent"
  (products :: [Molecule]) <- unrecord result "product"
  (inbound :: [REAGENT_IN]) <- unrecord result "reagent_in"
  (outbound :: [PRODUCT_FROM]) <- unrecord result "product_from"
  (accelerate :: [ACCELERATE]) <- unrecord result "accelerate"
  (catalysts :: [Catalyst]) <- unrecord result "catalyst"
  return $
    ReactionDetails
      {reaction, reagents, products, inbound, outbound, accelerate, catalysts}
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
