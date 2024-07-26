{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , loadConfig
  ) where

import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Monad        (unless)
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Text            (Text, pack)
import           System.Environment   (lookupEnv)

data Config =
  Config
    { dbUser     :: Text
    , dbPassword :: Text
    }

loadConfig :: IO Config
loadConfig = do
  loadFile defaultConfig
  maybeUser <- lookupEnv "NEO4J_USER"
  maybePassword <- lookupEnv "NEO4J_PASSWORD"
  let user = fromMaybe "neo4j" maybeUser
  let password = fromMaybe "password" maybePassword
  unless
    (isJust maybeUser)
    (putStrLn "Warning: NEO4J_USER not set. Using default value.")
  unless
    (isJust maybePassword)
    (putStrLn "Warning: NEO4J_PASSWORD not set. Using default value.")
  return $ Config {dbUser = pack user, dbPassword = pack password}
