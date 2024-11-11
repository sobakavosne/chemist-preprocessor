{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module defining the API endpoints for the application using the Servant library.

This module provides handlers for various endpoints that interact with 
the underlying domain services to perform operations related to reactions, 
mechanisms, paths, and health checks of the Neo4j database.

==== Reactions
  @GET \/reaction\/{id}@ 
  
  Fetches details of a reaction by its identifier.

      __Parameters:__

      - `id`: `ReactionID` - The unique identifier (`Int`) of the reaction to retrieve.

      __Response Body:__ `ReactionDetails`

  @POST \/reaction@
  
  Creates a new reaction with the provided details.

      __Body:__

      - `details`: `ReactionDetails` - The details of the reaction to create.

      __Response:__ `Reaction`

  @DELETE \/reaction\/{id}@
  
  Deletes a reaction by its identifier.

      __Parameters:__

      - `id`: `ReactionID` - The unique identifier of the reaction to delete (`Int`).

      __Response:__ `ReactionID`

==== Paths
  @GET \/path\/{start}\/{end}@
  
  Retrieves the shortest path between two molecules.

      __Parameters:__

      - `start`: `MoleculeID` - The starting molecule identifier (`Int`).
      - `end`: `MoleculeID` - The ending molecule identifier (`Int`).

      __Response:__ `PathMask`

==== Mechanisms
  @GET \/mechanism\/{id}@
  
  Fetches details of a mechanism by its identifier.

      __Parameters:__

      - `id`: `MechanismID` - The unique identifier (`Int`) of the mechanism to retrieve.

      __Response:__ `MechanismDetails`

==== Processes
  @GET \/process\/{id}@
  
  Retrieves complete information about a reaction, including its mechanism, if available.

      __Parameters:__
      
      - `id`: `ReactionID` - The unique identifier (`Int`) of the reaction to retrieve complete information.

      __Response:__ `ProcessDetails`

==== Checks
  @GET \/health@
  
  Returns the health status of the Neo4j database.

      __Response:__ `HealthCheck`
-}
module API.Endpoints
  ( api
  , server
  ) where

import           API.Error                (healthError, mismatchError,
                                           serviceError, toEither)
import           API.Logger               (log)
import           Control.Concurrent.Async (async, wait)
import           Control.Exception        (try)
import           Control.Monad            ((<=<))
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Domain.Service           (deleteReactionAsync, getHealthAsync,
                                           getMechanismAsync, getPathAsync,
                                           getReactionAsync, postReactionAsync)
import           Models                   (HealthCheck (..), MechanismDetails,
                                           MechanismID, MoleculeID, PathMask,
                                           ProcessDetails (..), Reaction,
                                           ReactionDetails, ReactionID)
import           Prelude                  hiding (id, log)
import qualified Servant                  as S

-- | Alias for **Content-Type** header
type Content = S.Headers '[ S.Header "Content-Type" String]

type API =
  "health"    S.:>                                                                   S.Get    '[ S.JSON] (Content HealthCheck) S.:<|>
  "reaction"  S.:> S.Capture "id" ReactionID S.:>                                    S.Get    '[ S.JSON] (Content ReactionDetails) S.:<|>
  "reaction"  S.:> S.ReqBody '[ S.JSON] ReactionDetails S.:>                         S.Post   '[ S.JSON] (Content Reaction) S.:<|>
  "reaction"  S.:> S.Capture "id" ReactionID S.:>                                    S.Delete '[ S.JSON] (Content ReactionID) S.:<|>
  "path"      S.:> S.Capture "start" MoleculeID S.:> S.Capture "end" MoleculeID S.:> S.Get    '[ S.JSON] (Content PathMask) S.:<|>
  "mechanism" S.:> S.Capture "id" MechanismID S.:>                                   S.Get    '[ S.JSON] (Content MechanismDetails) S.:<|>
  "process"   S.:> S.Capture "id" ReactionID S.:>                                    S.Get    '[ S.JSON] (Content ProcessDetails)

api :: S.Proxy API
api = S.Proxy

json :: String
json = "application/json"

-- | Handles requests to the health check endpoint. Returns a `JSON` response
-- containing a `HealthCheck` object with the status of the Neo4j server.
--
-- __Returns:__
--
-- - @S.Handler (Content HealthCheck)@
getHealthHandler :: S.Handler (Content HealthCheck)
getHealthHandler = do
  result <- liftIO . try $ wait =<< async getHealthAsync
  (liftIO . log) result
  either healthError (return . S.addHeader json) result

-- | Handles requests to get reaction details by its identifier. Returns a `JSON`
-- response containing `ReactionDetails` for the specified reaction ID.
--
-- __Parameters:__
--
-- * `ReactionID` - the unique identifier of the reaction
--
-- __Returns:__
--
-- - @S.Handler (Content ReactionDetails)@
getReactionHandler :: ReactionID -> S.Handler (Content ReactionDetails)
getReactionHandler id = do
  result <- liftIO . try $ wait =<< (async . fmap fst . getReactionAsync) id
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to create a new reaction with the provided details. Returns
-- a `JSON` response with the created `Reaction`.
--
-- __Parameters:__
--
-- * `ReactionDetails` - details of the reaction to be created.
--   Note: `ACCELERATE` and `Catalyst` are optional. If they are not provided,
--   default values will be used.
--
-- __Returns:__
--
-- * @S.Handler (Content Reaction)@
postReactionHandler :: ReactionDetails -> S.Handler (Content Reaction)
postReactionHandler details = do
  result <- liftIO . try $ wait =<< (async . postReactionAsync) details
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to delete a reaction by its identifier. Returns a `JSON`
-- response with the ID of the deleted reaction.
--
-- __Parameters:__
--
-- * `ReactionID` - the unique identifier of the reaction to be deleted
--
-- __Returns:__
-- 
-- * @S.Handler (Content ReactionID)@
deleteReactionHandler :: ReactionID -> S.Handler (Content ReactionID)
deleteReactionHandler id = do
  result <- liftIO . try $ wait =<< (async . deleteReactionAsync) id
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to get the shortest path from one molecule to another through reactions
-- and molecules. Requires `start` and `end` molecule IDs.
--
-- __Parameters:__
--
-- * `MoleculeID` - the starting molecule ID
-- * `MoleculeID` - the ending molecule ID
--
-- __Returns:__
--
-- * @S.Handler (Content PathMask)@
getPathHandler :: MoleculeID -> MoleculeID -> S.Handler (Content PathMask)
getPathHandler start end = do
  result <- liftIO . try $ wait =<< async (getPathAsync start end)
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to get the details of a mechanism by its identifier. Returns a `JSON`
-- response containing `MechanismDetails` for the specified mechanism ID.
--
-- __Parameters:__
--
-- * `MechanismID` - the unique identifier of the mechanism
--
-- __Returns:__
--
-- * @S.Handler (Content MechanismDetails)@
getMechanismHandler :: MechanismID -> S.Handler (Content MechanismDetails)
getMechanismHandler id = do
  result <- liftIO . try $ wait =<< (async . getMechanismAsync) id
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to get full information about a reaction, including its mechanism
-- if present. Returns a `JSON` response containing `ProcessDetails` for the specified reaction ID.
--
-- __Parameters:__
--
-- * `ReactionID` - the unique identifier of the reaction
--
-- __Returns:__
--
-- * @S.Handler (Content ProcessDetails)@
getProcessDetailsHandler :: ReactionID -> S.Handler (Content ProcessDetails)
getProcessDetailsHandler id = do
  reaction <- liftIO . try $ wait =<< (async . getReactionAsync) id
  mechanism <-
    either
      mismatchError
      (traverse (liftIO . try . (wait <=< (async . getMechanismAsync))) . snd)
      reaction
  let result = (ProcessDetails . fst <$> reaction) <*> toEither mechanism
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

server :: S.Server API
server =
  getHealthHandler S.:<|>
  getReactionHandler S.:<|>
  postReactionHandler S.:<|>
  deleteReactionHandler S.:<|>
  getPathHandler S.:<|>
  getMechanismHandler S.:<|>
  getProcessDetailsHandler
