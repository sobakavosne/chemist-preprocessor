module Domain.Service
  ( getPathAsync
  , getHealthAsync
  , getReactionAsync
  , getMechanismAsync
  , postReactionAsync
  , deleteReactionAsync
  ) where

import           Control.Concurrent.Async   (async, wait)
import           Domain.Converter.Converter (toMechanismDetails, toPath,
                                             toRawReactionDetails, toReaction,
                                             toReactionDetails)
import           Infrastructure.Database    (checkNeo4j, createReaction,
                                             fetchMechanism, fetchReaction,
                                             findPath, removeReaction,
                                             withNeo4j)
import           Models                     (HealthCheck, MechanismDetails,
                                             MechanismID, MoleculeID, PathMask,
                                             Reaction, ReactionDetails,
                                             ReactionID)
import           Prelude                    hiding (id)

-- | Asynchronously fetch the health status of the Neo4j database.
-- Uses the `withNeo4j` wrapper to establish a connection
-- and execute the health check query.
--
-- Returns:
-- - @HealthCheck@ containing the status information of the database.
getHealthAsync :: IO HealthCheck
getHealthAsync = wait =<< (async . withNeo4j) checkNeo4j

-- | Asynchronously fetches the details of a reaction based on its unique @ReactionID@.
-- Converts the raw reaction data retrieved from the database into
-- @ReactionDetails@ format.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction.
--
-- Returns:
-- - A tuple of @ReactionDetails@ and an optional @MechanismID@, if a mechanism is associated.
getReactionAsync :: ReactionID -> IO (ReactionDetails, Maybe MechanismID)
getReactionAsync id =
  toReactionDetails =<< wait =<< (async . withNeo4j . fetchReaction) id

-- | Asynchronously creates a new reaction in the database.
-- Converts the given @ReactionDetails@ to raw details before calling the
-- database function to store the reaction.
--
-- Parameters:
-- - @ReactionDetails@ - the details of the reaction to be created.
--
-- Returns:
-- - @Reaction@ - the created reaction with its unique ID.
postReactionAsync :: ReactionDetails -> IO Reaction
postReactionAsync details =
  toReaction =<<
  wait =<< async . withNeo4j . createReaction =<< toRawReactionDetails details

-- | Asynchronously deletes a reaction from the database based on its @ReactionID@.
-- Uses `withNeo4j` to execute the delete operation.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction to be deleted.
--
-- Returns:
-- - @ReactionID@ of the deleted reaction.
deleteReactionAsync :: ReactionID -> IO ReactionID
deleteReactionAsync id = wait =<< (async . withNeo4j . removeReaction) id

-- | Asynchronously finds the shortest path between two molecules based on their @MoleculeID@s.
-- Fetches the path from the database and converts it to a @PathMask@.
--
-- Parameters:
-- - @MoleculeID@ - the starting molecule's ID.
-- - @MoleculeID@ - the ending molecule's ID.
--
-- Returns:
-- - @PathMask@ representing the shortest path between the two molecules.
getPathAsync :: MoleculeID -> MoleculeID -> IO PathMask
getPathAsync start end = toPath =<< wait =<< (async . withNeo4j) (findPath start end)

-- | Asynchronously  fetches the details of a mechanism based on its @MechanismID@.
-- Converts the raw mechanism data into @MechanismDetails@.
--
-- Parameters:
-- - @MechanismID@ - the unique identifier of the mechanism.
--
-- Returns:
-- - @MechanismDetails@ representing the mechanism and its stages.
getMechanismAsync :: MechanismID -> IO MechanismDetails
getMechanismAsync id =
  toMechanismDetails =<< wait =<< (async . withNeo4j . fetchMechanism) id
