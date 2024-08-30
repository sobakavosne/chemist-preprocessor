module Domain.Service where

import           Domain.Type             (convert)
import           Infrastructure.Config   (loadBoltCfg)
import           Infrastructure.Database (fetchReaction, withNeo4j)
import           Models                  (ReactionDetails)
import           Prelude                 hiding (id)

getReaction :: Int -> IO ReactionDetails
getReaction id = convert =<< withNeo4j (fetchReaction id) =<< loadBoltCfg
