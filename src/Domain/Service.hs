module Domain.Service
  ( getPath
  , getReaction
  , getMechanism
  , postReaction
  , deleteReaction
  ) where

import           Domain.Converter.Converter (toMechanismDetails, toPath,
                                             toRawDetails, toReaction,
                                             toReactionDetails)
import           Infrastructure.Database    (createReaction, fetchMechanism,
                                             fetchReaction, findPath,
                                             removeReaction, withNeo4j)
import           Models                     (MechanismDetails, PathMask,
                                             Reaction, ReactionDetails,
                                             ReactionId)
import           Prelude                    hiding (id)

getReaction :: Int -> IO ReactionDetails
getReaction id = toReactionDetails =<< withNeo4j (fetchReaction id)

postReaction :: ReactionDetails -> IO Reaction
postReaction details = do
  rawDetailsMask <- toRawDetails details
  toReaction =<< withNeo4j (createReaction rawDetailsMask)

deleteReaction :: Int -> IO Int
deleteReaction id = withNeo4j (removeReaction id)

getPath :: Int -> Int -> IO PathMask
getPath start end = toPath =<< withNeo4j (findPath start end)

getMechanism :: Int -> IO (MechanismDetails, ReactionId)
getMechanism id = toMechanismDetails =<< withNeo4j (fetchMechanism id)
