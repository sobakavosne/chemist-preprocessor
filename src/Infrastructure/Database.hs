{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions for interacting with a Neo4j graph database.
--   It includes operations for fetching, creating, and removing reactions and mechanisms,
--   as well as checking the health of the Neo4j server.
-- 
-- ==== Functions
-- 
-- * `checkNeo4j`: Checks the health of the Neo4j server and returns a `HealthCheck` structure indicating the server status.
-- * `fetchReaction`: Retrieves details of a reaction by its `ReactionID`. Returns a tuple containing `RawReactionDetails` and an optional `MechanismNode`.
-- * `createReaction`: Creates a new reaction in the database using the provided `RawReactionDetailsMask`. Returns the created `ReactionNode`.
-- * `removeReaction`: Removes a reaction and its associated nodes from the database based on the provided `ReactionID`. Returns the ID of the removed reaction.
-- * `findPath`: Finds the shortest path between two molecules specified by their IDs. Returns a `Path` representing the shortest path.
-- * `fetchMechanism`: Fetches mechanism details by its identifier from the Neo4j database. Returns a `RawMechanismDetails` structure representing the mechanism and its stages.
-- * `withNeo4j`: Executes a given `BoltActionT` action within a Neo4j database context, handling connection opening and closing.
--
-- ==== Error Handling
-- 
-- This module defines a custom exception type `GraphElemError` to handle errors related to missing nodes or relationships.
-- The function `headIO` safely retrieves the head element from a list, throwing an exception if the list is empty.
--
module Infrastructure.Database
  ( findPath
  , withNeo4j
  , checkNeo4j
  , fetchReaction
  , createReaction
  , removeReaction
  , fetchMechanism
  ) where

import           Control.Exception             (Exception, bracket, throw,
                                                throwIO, try)
import           Control.Monad.Except          (MonadError (catchError),
                                                MonadTrans (lift), forM)
import           Data.Text                     (Text)
import           Database.Bolt                 (BoltActionT, Node, Path, Record,
                                                RecordValue, Relationship,
                                                Value (I), at, close, connect,
                                                props, query, queryP, run)
import           GHC.Exception                 (SomeException)
import           Infrastructure.Config         (loadBoltCfg)
import           Infrastructure.QueryGenerator (createReactionQueryFrom)
import           Models                        (HealthCheck (HealthCheck),
                                                MechanismID, MechanismNode,
                                                MoleculeID,
                                                RawMechanismDetails (..),
                                                RawReactionDetails (..),
                                                RawReactionDetailsMask (..),
                                                ReactionID, ReactionNode)
import           Prelude                       hiding (id)

newtype GraphElemError =
  GraphElemError Text
  deriving (Eq, Show)

instance Exception GraphElemError

headIO :: Text -> [a] -> IO a
headIO key [] =
  throwIO
    (GraphElemError $
     "Expected non-empty Record list retrieved by the key: " <> key)
headIO _ (x:_) = return x

unrecord :: RecordValue b => [Record] -> Text -> BoltActionT IO b
unrecord result key = (lift . headIO key) =<< forM result (`at` key)

-- | Executes a given BoltActionT action within a Neo4j database context.
--   Automatically handles connection opening and closing.
--
--   ==== Parameters
--  
--   * @BoltActionT IO b@ - the action to execute.
--  
--   ==== Returns
--  
--   * @IO b@ - the result of the executed action.
--  
--   ==== Exceptions
--  
--   * @BoltError@ if the connection to the database fails or if the action encounters an error.

withNeo4j :: forall b. BoltActionT IO b -> IO b
withNeo4j action = do
  cfg <- loadBoltCfg
  result <-
    try (bracket (connect cfg) close (`run` action)) :: IO (Either SomeException b)
  either throwIO return result

-- | Check the health of the Neo4j server by running a simple query.
--
-- ==== Returns
--
-- * A `HealthCheck` structure indicating server status.
checkNeo4j :: BoltActionT IO HealthCheck
checkNeo4j = do
  _ <- query "RETURN 1" `catchError` throw
  return $ HealthCheck "Neo4j is alive" "Server is alive"

-- | Fetches reaction details by its identifier from the Neo4j database.
--   Returns a tuple containing `RawReactionDetails` and optionally `MechanismNode`.
--
--   ==== Parameters
--  
--   * `ReactionID` - the unique identifier of the reaction.
--  
--   ==== Returns
--  
--   * `BoltActionT IO (RawReactionDetails, Maybe MechanismNode)`.
--  
--   ==== Exceptions
--  
--   * `GraphElemError` if the expected nodes or relationships are not found.
--   * `BoltError`
fetchReaction ::
     ReactionID -> BoltActionT IO (RawReactionDetails, Maybe MechanismNode)
fetchReaction id = do
  result <-
    queryP
      "MATCH \
             \(reaction:Reaction { id: $id })<-[accelerate:ACCELERATE]-(catalyst:Catalyst), \
             \(reaction)-[product_from:PRODUCT_FROM]->(product:Molecule), \
             \(reaction)<-[reagent_in:REAGENT_IN]-(reagent:Molecule) \
             \OPTIONAL MATCH (reaction)-[follow:FOLLOW]->(mechanism:Mechanism) \
             \RETURN DISTINCT reaction, \
             \  mechanism, \
             \  COLLECT(DISTINCT accelerate) AS accelerates, \
             \  COLLECT(DISTINCT catalyst) AS catalysts, \
             \  COLLECT(DISTINCT product) AS products, \
             \  COLLECT(DISTINCT reagent) AS reagents, \
             \  COLLECT(DISTINCT product_from) AS product_froms, \
             \  COLLECT(DISTINCT reagent_in) AS reagent_ins"
      (props [("id", I id)])
  (rawReaction   :: Node)           <- unrecord result "reaction"
  (rawReagents   :: [Node])         <- unrecord result "reagents"
  (rawInbound    :: [Relationship]) <- unrecord result "reagent_ins"
  (rawProducts   :: [Node])         <- unrecord result "products"
  (rawOutbound   :: [Relationship]) <- unrecord result "product_froms"
  (rawAccelerate :: [Relationship]) <- unrecord result "accelerates"
  (rawCatalysts  :: [Node])         <- unrecord result "catalysts"
  (rawMechanism  :: Maybe Node)     <- unrecord result "mechanism"
  return
    ( RawReactionDetails
        { rawReaction
        , rawReagents
        , rawProducts
        , rawInbound
        , rawOutbound
        , rawAccelerate
        , rawCatalysts
        }
    , rawMechanism)

-- | Creates a new reaction in the Neo4j database using the provided details.
--   Generates a query to insert a reaction node and related entities.
--
--   ==== Parameters
--  
--   * `RawReactionDetailsMask` - containing the details for the new reaction.
--  
--   ==== Returns
--  
--   * The created `ReactionNode`.
--  
--   ==== Exceptions
--  
--   * `GraphElemError` if the reaction creation fails.
--   * `BoltError`
createReaction :: RawReactionDetailsMask -> BoltActionT IO ReactionNode
createReaction details = do
  result <- query $ createReactionQueryFrom details
  (rawReaction :: Node) <- unrecord result "reaction"
  return rawReaction

-- | Removes a reaction and its associated nodes from the Neo4j database based on the provided ReactionID.
--
--   ==== Parameters
--
--   * `ReactionID` - the unique identifier of the reaction to remove.
--
--   ==== Returns
--
--   * The ID of the removed reaction.
--
--   ==== Exceptions
--
--   * `BoltError`
removeReaction :: ReactionID -> BoltActionT IO ReactionID
removeReaction id = do
  _ <-
    queryP
      "MATCH \
             \(reaction:Reaction { id: $id })<-[accelerate:ACCELERATE]-(catalyst:Catalyst), \
             \(reaction)-[product_from:PRODUCT_FROM]->(product:Molecule), \
             \(reaction)<-[reagent_in:REAGENT_IN]-(reagent:Molecule) \
             \DELETE reaction, accelerate, catalyst, product, reagent, product_from, reagent_in"
      (props [("id", I id)])
  return id

-- | Finds the shortest path between two molecules based on their IDs.
--   The path consists of reactions and relationships between products and reagents.
--
--   ==== Parameters
--  
--   * `MoleculeID` - the unique identifier of the starting molecule.
--   * `MoleculeID` - the unique identifier of the ending molecule.
--  
--   ==== Returns
--  
--   * A `Path` representing the shortest path between the two molecules.
--  
--   ==== Exceptions
--  
--   * `GraphElemError` if no path is found between the two molecules.
--   * `BoltError`
findPath :: MoleculeID -> MoleculeID -> BoltActionT IO Path
findPath startId endId = do
  result <-
    queryP
      "MATCH \
             \path = shortestPath((start:Reaction {id: $start})\
             \ -[:PRODUCT_FROM|REAGENT_IN*]\
             \ -(end:Reaction {id: $end}))\
             \RETURN path"
      (props [("start", I startId), ("end", I endId)])
  (rawPath :: Path) <- unrecord result "path"
  return rawPath

-- | Fetches mechanism details by its identifier from the Neo4j database.
--   Retrieves the mechanism, its stages, participants, and relationships.
--
--   ==== Parameters
--
--   * `MechanismID` - the unique identifier of the mechanism.
--
--   ==== Returns
--
--   * A `RawMechanismDetails` representing the mechanism and its stages.
--
--   ==== Exceptions
--
--   * `GraphElemError` if the expected nodes or relationships are not found.
--   * `BoltError`
fetchMechanism :: MechanismID -> BoltActionT IO RawMechanismDetails
fetchMechanism id = do
  result <-
    queryP
      "MATCH \
             \(reaction:Reaction)-[follow:FOLLOW]->(mechanism:Mechanism { id: $id}), \
             \(mechanism)-[has_stage:HAS_STAGE]->(stage:Stage), \
             \(stage)<-[include:INCLUDE]-(participant) \
             \RETURN DISTINCT mechanism, \
             \  reaction, \
             \  follow, \
             \  COLLECT(DISTINCT has_stage) AS has_stages, \
             \  COLLECT(DISTINCT stage) AS stages, \
             \  COLLECT(DISTINCT include) AS includes, \
             \  COLLECT(DISTINCT participant) AS participants"
      (props [("id", I id)])
  (rawMechanism    :: Node)           <- unrecord result "mechanism"
  (rawStages       :: [Node])         <- unrecord result "stages"
  (rawInclude      :: [Relationship]) <- unrecord result "includes"
  (rawInteractants :: [Node])         <- unrecord result "participants"
  (rawFollow       :: Relationship)   <- unrecord result "follow"
  return
    RawMechanismDetails
      {rawMechanism, rawStages, rawInclude, rawInteractants, rawFollow}
