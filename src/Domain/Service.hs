module Domain.Service where

import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Infrastructure.Config   (loadBoltCfg)
import           Infrastructure.Database (fetchReaction, withNeo4j)
import           Infrastructure.Type     (convert)
import           Models                  (ReactionDetails)
import           Prelude                 hiding (id)

getReaction :: Int -> IO ReactionDetails
getReaction id =
  convert <$> (liftIO . withNeo4j (fetchReaction id) =<< liftIO loadBoltCfg)
