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
import           Infrastructure.Config      (loadBoltCfg)
import           Infrastructure.Database    (createReaction, fetchMechanism,
                                             fetchReaction, findPath,
                                             removeReaction, withNeo4j)
import           Models                     (MechanismDetails, PathMask,
                                             Reaction, ReactionDetails)
import           Prelude                    hiding (id)

getReaction :: Int -> IO ReactionDetails
getReaction id =
  toReactionDetails =<< withNeo4j (fetchReaction id) =<< loadBoltCfg

postReaction :: ReactionDetails -> IO Reaction
postReaction details = do
  rawDetailsMask <- toRawDetails details
  toReaction =<< withNeo4j (createReaction rawDetailsMask) =<< loadBoltCfg

deleteReaction :: Int -> IO ()
deleteReaction id = withNeo4j (removeReaction id) =<< loadBoltCfg

getPath :: Int -> Int -> IO PathMask
getPath start end = toPath =<< withNeo4j (findPath start end) =<< loadBoltCfg

getMechanism :: Int -> IO MechanismDetails
getMechanism id =
  toMechanismDetails =<< withNeo4j (fetchMechanism id) =<< loadBoltCfg
