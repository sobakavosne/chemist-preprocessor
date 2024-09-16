module Domain.Service
  ( getPath
  , getHealth
  , getReaction
  , getMechanism
  , postReaction
  , deleteReaction
  ) where

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

-- | Fetch the health status of the Neo4j database.
-- Uses the `withNeo4j` wrapper to establish a connection
-- and execute the health check query.
--
-- Returns:
-- - @HealthCheck@ containing the status information of the database.
getHealth :: IO HealthCheck
getHealth = withNeo4j checkNeo4j

-- | Fetches the details of a reaction based on its unique @ReactionID@.
-- Converts the raw reaction data retrieved from the database into
-- @ReactionDetails@ format.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction.
--
-- Returns:
-- - A tuple of @ReactionDetails@ and an optional @MechanismID@, if a mechanism is associated.
getReaction :: ReactionID -> IO (ReactionDetails, Maybe MechanismID)
getReaction id = toReactionDetails =<< withNeo4j (fetchReaction id)

-- | Creates a new reaction in the database.
-- Converts the given @ReactionDetails@ to raw details before calling the
-- database function to store the reaction.
--
-- Parameters:
-- - @ReactionDetails@ - the details of the reaction to be created.
--
-- Returns:
-- - @Reaction@ - the created reaction with its unique ID.
postReaction :: ReactionDetails -> IO Reaction
postReaction details = toReaction =<< withNeo4j . createReaction =<< toRawReactionDetails details

-- | Deletes a reaction from the database based on its @ReactionID@.
-- Uses `withNeo4j` to execute the delete operation.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction to be deleted.
--
-- Returns:
-- - @ReactionID@ of the deleted reaction.
deleteReaction :: ReactionID -> IO ReactionID
deleteReaction id = withNeo4j (removeReaction id)

-- | Finds the shortest path between two molecules based on their @MoleculeID@s.
-- Fetches the path from the database and converts it to a @PathMask@.
--
-- Parameters:
-- - @MoleculeID@ - the starting molecule's ID.
-- - @MoleculeID@ - the ending molecule's ID.
--
-- Returns:
-- - @PathMask@ representing the shortest path between the two molecules.
getPath :: MoleculeID -> MoleculeID -> IO PathMask
getPath start end = toPath =<< withNeo4j (findPath start end)

-- | Fetches the details of a mechanism based on its @MechanismID@.
-- Converts the raw mechanism data into @MechanismDetails@.
-- 
-- Parameters:
-- - @MechanismID@ - the unique identifier of the mechanism.
-- 
-- Returns:
-- - @MechanismDetails@ representing the mechanism and its stages.
getMechanism :: MechanismID -> IO MechanismDetails
getMechanism id = toMechanismDetails =<< withNeo4j (fetchMechanism id)
