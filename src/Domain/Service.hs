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

getHealth :: IO HealthCheck
getHealth = withNeo4j checkNeo4j

getReaction :: ReactionID -> IO (ReactionDetails, Maybe MechanismID)
getReaction id = toReactionDetails =<< withNeo4j (fetchReaction id)

postReaction :: ReactionDetails -> IO Reaction
postReaction details = toReaction =<< withNeo4j . createReaction =<< toRawReactionDetails details

deleteReaction :: ReactionID -> IO ReactionID
deleteReaction id = withNeo4j (removeReaction id)

getPath :: MoleculeID -> MoleculeID -> IO PathMask
getPath start end = toPath =<< withNeo4j (findPath start end)

getMechanism :: MechanismID -> IO MechanismDetails
getMechanism id = toMechanismDetails =<< withNeo4j (fetchMechanism id)
