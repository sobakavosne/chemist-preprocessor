{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Database
  ( withNeo4j
  , checkNeo4j
  , fetchReaction
  , createReaction
  , removeReaction
  ) where

import           Control.Exception             (Exception, throwIO, try)
import           Control.Monad.Except          (MonadError (catchError),
                                                MonadTrans (lift), forM)
import           Data.Text                     (Text)
import           Database.Bolt                 (BoltActionT, BoltCfg, BoltError,
                                                Node, Pipe, Record, RecordValue,
                                                Relationship, Value (I), at,
                                                connect, props, query, queryP,
                                                run)
import           Infrastructure.QueryGenerator (createReactionQueryFrom)
import           Models                        (RawReactionDetails (..),
                                                RawReactionDetailsMask (..))
import           Prelude                       hiding (head, id)

newtype GraphElemError =
  GraphElemError Text
  deriving (Eq, Show)

instance Exception GraphElemError

-- | Safe `head` which throws `GraphElemError` if list is empty
head :: [a] -> IO a
head []    = throwIO (GraphElemError "Expected non-empty list")
head (x:_) = return x

-- | Unpack the list of `result` records with `key`
unrecord ::
     (Traversable t, Monad m, RecordValue b)
  => t Record
  -> Text
  -> BoltActionT m (t b)
unrecord result key = forM result (`at` key)

withNeo4j :: BoltActionT IO b -> BoltCfg -> IO b
withNeo4j action cfg = do
  pipe <- try (connect cfg) :: IO (Either BoltError Pipe)
  either throwIO (`run` action) pipe

checkNeo4j :: BoltActionT IO Bool
checkNeo4j = do
  result <- query "RETURN 1" `catchError` handleException
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
  (rawReaction   :: Node)           <- (lift . head) =<< unrecord result "reaction"
  (rawReagents   :: [Node])         <- (lift . head) =<< unrecord result "reagents"
  (rawInbound    :: [Relationship]) <- (lift . head) =<< unrecord result "reagent_ins"
  (rawProducts   :: [Node])         <- (lift . head) =<< unrecord result "products"
  (rawOutbound   :: [Relationship]) <- (lift . head) =<< unrecord result "product_froms"
  (rawAccelerate :: [Relationship]) <- (lift . head) =<< unrecord result "accelerates"
  (rawCatalysts  :: [Node])         <- (lift . head) =<< unrecord result "catalysts"
  return
    RawDetails
      { rawReaction
      , rawReagents
      , rawProducts
      , rawInbound
      , rawOutbound
      , rawAccelerate
      , rawCatalysts
      }

createReaction :: RawReactionDetailsMask -> BoltActionT IO Node
createReaction details = do
  result <- query $ createReactionQueryFrom details
  (rawReaction :: Node) <- (lift . head) =<< unrecord result "reaction"
  return rawReaction

removeReaction :: Int -> BoltActionT IO ()
removeReaction id = do
  _ <-
    queryP
      "MATCH \
             \(reaction:Reaction { id: $id })<-[accelerate:ACCELERATE]-(catalyst:Catalyst), \
             \(reaction)-[product_from:PRODUCT_FROM]->(product:Molecule), \
             \(reaction)<-[reagent_in:REAGENT_IN]-(reagent:Molecule) \
             \DELETE reaction, accelerate, catalyst, product, reagent, product_from, reagent_in"
      (props [("id", I id)])
  pure ()
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
