module Domain.Service
  ( getReaction
  , postReaction
  , deleteReaction
  ) where

import           Domain.Converter.Converter (toDetails, toRawDetails,
                                             toReaction)
import           Infrastructure.Config      (loadBoltCfg)
import           Infrastructure.Database    (createReaction, fetchReaction,
                                             removeReaction, withNeo4j)
import           Models                     (Reaction, ReactionDetails)
import           Prelude                    hiding (id)

getReaction :: Int -> IO ReactionDetails
getReaction id = toDetails =<< withNeo4j (fetchReaction id) =<< loadBoltCfg

postReaction :: ReactionDetails -> IO Reaction
postReaction details = do
  rawDetailsMask <- toRawDetails details
  toReaction =<< withNeo4j (createReaction rawDetailsMask) =<< loadBoltCfg

deleteReaction :: Int -> IO ()
deleteReaction id = withNeo4j (removeReaction id) =<< loadBoltCfg
