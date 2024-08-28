{-# LANGUAGE FlexibleInstances #-}

module Domain.Service where

import           Control.Monad.Except    (MonadIO (..))
import           Domain.Type             (ParsingError, convert)
import           Infrastructure.Config   (loadBoltCfg)
import           Infrastructure.Database (fetchReaction, withNeo4j)
import           Models                  (ReactionDetails)
import           Prelude                 hiding (id)

getReaction :: Int -> Either ParsingError ReactionDetails
getReaction id =
  convert =<< (liftIO . withNeo4j (fetchReaction id) =<< liftIO loadBoltCfg)
