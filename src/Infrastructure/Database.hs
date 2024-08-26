{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Database
  ( withNeo4j
  , checkNeo4j
  , fetchReaction
  ) where

import           Control.Monad.Except   (MonadError (catchError))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (pack)
import           Database.Bolt          (BoltActionT, BoltCfg, BoltError, Node,
                                         Relationship, Value (I), connect,
                                         props, query, queryP, run)
import           Helpers                (unrecord)
import           Models                 (RawReactionDetails (..))
import           Prelude                hiding (id)

withNeo4j :: MonadIO m => BoltActionT m b -> BoltCfg -> m b
withNeo4j action cfg = do
  pipe <- connect cfg
  run pipe action

checkNeo4j :: BoltActionT IO Bool
checkNeo4j = do
  result <- query (pack "RETURN 1") `catchError` handleException
  return $ not (null result)
  where
    handleException :: BoltError -> BoltActionT IO [a]
    handleException _ = return []

fetchReaction :: Int -> BoltActionT IO RawReactionDetails
fetchReaction id = do
  result <-
    queryP
      "MATCH \
             \(reaction:Reaction { id: $id })<-[accelerate:ACCELERATE]-(catalyst:Catalyst), \
             \(reaction)-[product_from:PRODUCT_FROM]->(product:Molecule), \
             \(reaction)<-[reagent_in:REAGENT_IN]-(reagent:Molecule) \
             \RETURN DISTINCT reaction, \
             \  COLLECT(DISTINCT accelerate) AS accelerates, \
             \  COLLECT(DISTINCT catalyst) AS catalysts, \
             \  COLLECT(DISTINCT product) AS products, \
             \  COLLECT(DISTINCT reagent) AS reagents, \
             \  COLLECT(DISTINCT product_from) AS product_froms, \
             \  COLLECT(DISTINCT reagent_in) AS reagent_ins"
      (props [("id", I id)])
  (reaction :: Node) <- head <$> unrecord result "reaction"
  -- (reaction2 :: Either ParsingError Reaction) <- (lift . runExceptT . exact) . head =<< unrecord result "reaction"
  (reagents :: [Node]) <- head <$> unrecord result "reagents"
  (inbound :: [Relationship]) <- head <$> unrecord result "reagent_ins"
  (products :: [Node]) <- head <$> unrecord result "products"
  (outbound :: [Relationship]) <- head <$> unrecord result "product_froms"
  (accelerate :: [Relationship]) <- head <$> unrecord result "accelerates"
  (catalysts :: [Node]) <- head <$> unrecord result "catalysts"
  return
    RawDetails
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
