{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.Endpoints
  ( api
  , server
  ) where

import           API.Error              (healthError, mismatchError,
                                         serviceError, toEither)
import           API.Logger             (log)
import           Control.Exception      (try)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Domain.Service         (deleteReaction, getHealth,
                                         getMechanism, getPath, getReaction,
                                         postReaction)
import           Models                 (HealthCheck (..), MechanismDetails,
                                         MechanismID, MoleculeID, PathMask,
                                         ProcessDetails (..), Reaction,
                                         ReactionDetails, ReactionID)
import           Prelude                hiding (id, log)
import qualified Servant                as S

-- | Alias for **Content-Type** header
type Content = S.Headers '[ S.Header "Content-Type" String]

type API =
  "health"    S.:>                                                                   S.Get    '[ S.JSON] (Content HealthCheck) S.:<|>
  "reaction"  S.:> S.Capture "id" ReactionID S.:>                                    S.Get    '[ S.JSON] (Content ReactionDetails) S.:<|>
  "reaction"  S.:> S.ReqBody '[ S.JSON] ReactionDetails S.:>                         S.Post   '[ S.JSON] (Content Reaction) S.:<|>
  "reaction"  S.:> S.Capture "id" ReactionID S.:>                                    S.Delete '[ S.JSON] (Content ReactionID) S.:<|>
  "path"      S.:> S.Capture "start" MoleculeID S.:> S.Capture "end" MoleculeID S.:> S.Get    '[ S.JSON] (Content PathMask) S.:<|>
  "mechanism" S.:> S.Capture "id" MechanismID S.:>                                   S.Get    '[ S.JSON] (Content MechanismDetails) S.:<|>
  "process"   S.:> S.Capture "reactionId" ReactionID S.:>                            S.Get    '[ S.JSON] (Content ProcessDetails)

api :: S.Proxy API
api = S.Proxy

json :: String
json = "application/json"

-- | Handles requests to the health check endpoint. Returns a `JSON` response
-- containing a @HealthCheck@ object with the status of the Neo4j server.
--
-- Returns:
-- - @S.Handler (Content HealthCheck)@
healthHandler :: S.Handler (Content HealthCheck)
healthHandler = do
  result <- (liftIO . try) getHealth
  (liftIO . log) result
  either healthError (return . S.addHeader json) result

-- | Handles requests to get reaction details by its identifier. Returns a JSON
-- response containing @ReactionDetails@ for the specified reaction ID.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction
--
-- Returns:
-- - @S.Handler (Content ReactionDetails)@
getReactionHandler :: ReactionID -> S.Handler (Content ReactionDetails)
getReactionHandler id = do
  result <- (liftIO . try . fmap fst . getReaction) id
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to create a new reaction with the provided details. Returns
-- a `JSON` response with the created @Reaction@.
--
-- Parameters:
-- - @ReactionDetails@ - details of the reaction to be created. 
--   Note: @ACCELERATE@ and @Catalyst@ are optional. If they are not provided, 
--   default values will be used.
--
-- Returns:
-- - @S.Handler (Content Reaction)@
postReactionHandler :: ReactionDetails -> S.Handler (Content Reaction)
postReactionHandler details = do
  result <- (liftIO . try . postReaction) details
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to delete a reaction by its identifier. Returns a `JSON`
-- response with the ID of the deleted reaction.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction to be deleted
--
-- Returns:
-- - @S.Handler (Content ReactionID)@
deleteReactionHandler :: ReactionID -> S.Handler (Content ReactionID)
deleteReactionHandler id = do
  result <- (liftIO . try . deleteReaction) id
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to get the shortest path from one molecule to another through reactions
-- and molecules. Requires `start` and `end` molecule IDs.
--
-- Parameters:
-- - @MoleculeID@ - the starting molecule ID
-- - @MoleculeID@ - the ending molecule ID
--
-- Returns:
-- - @S.Handler (Content PathMask)@
getPathHandler :: MoleculeID -> MoleculeID -> S.Handler (Content PathMask)
getPathHandler start end = do
  result <- (liftIO . try . getPath start) end
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to get the details of a mechanism by its identifier. Returns a `JSON`
-- response containing @MechanismDetails@ for the specified mechanism ID.
--
-- Parameters:
-- - @MechanismID@ - the unique identifier of the mechanism
--
-- Returns:
-- - @S.Handler (Content MechanismDetails)@
getMechanismHandler :: MechanismID -> S.Handler (Content MechanismDetails)
getMechanismHandler id = do
  result <- (liftIO . try . getMechanism) id
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

-- | Handles requests to get full information about a reaction, including its mechanism
-- if present. Returns a `JSON` response containing @ProcessDetails@ for the specified reaction ID.
--
-- Parameters:
-- - @ReactionID@ - the unique identifier of the reaction
--
-- Returns:
-- - @S.Handler (Content ProcessDetails)@
getProcessDetailsHandler :: ReactionID -> S.Handler (Content ProcessDetails)
getProcessDetailsHandler reactionId = do
  reaction <- (liftIO . try . getReaction) reactionId
  mechanism <-
    either mismatchError (traverse (liftIO . try . getMechanism) . snd) reaction
  let result = (ProcessDetails . fst <$> reaction) <*> toEither mechanism
  (liftIO . log) result
  either serviceError (return . S.addHeader json) result

server :: S.Server API
server =
  healthHandler S.:<|> getReactionHandler S.:<|> postReactionHandler S.:<|>
  deleteReactionHandler S.:<|>
  getPathHandler S.:<|>
  getMechanismHandler S.:<|>
  getProcessDetailsHandler
