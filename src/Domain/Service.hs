module Domain.Service
  ( getPath
  , getReaction
  , postReaction
  , deleteReaction
  ) where

import           Domain.Converter.Converter (toDetails, toPath, toRawDetails,
                                             toReaction)
import           Infrastructure.Config      (loadBoltCfg)
import           Infrastructure.Database    (createReaction, fetchReaction,
                                             findPath, removeReaction,
                                             withNeo4j)
import           Models                     (PathMask, Reaction,
                                             ReactionDetails)
import           Prelude                    hiding (id)

getReaction :: Int -> IO ReactionDetails
getReaction id = toDetails =<< withNeo4j (fetchReaction id) =<< loadBoltCfg

postReaction :: ReactionDetails -> IO Reaction
postReaction details = do
  rawDetailsMask <- toRawDetails details
  toReaction =<< withNeo4j (createReaction rawDetailsMask) =<< loadBoltCfg

deleteReaction :: Int -> IO ()
deleteReaction id = withNeo4j (removeReaction id) =<< loadBoltCfg

getPath :: Int -> Int -> IO PathMask
getPath start end = toPath =<< withNeo4j (findPath start end) =<< loadBoltCfg
