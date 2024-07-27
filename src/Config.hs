{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , loadConfig
  ) where

import qualified Configuration.Dotenv as Dotenv
import           Control.Monad        (unless)
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Text            (Text, pack)
import           System.Environment   (lookupEnv)

data Config =
  Config
    { dbUser     :: Text
    , dbPassword :: Text
    , dbHost     :: Text
    , dbPort     :: Int
    }

defaultUser :: String
defaultUser = "neo4j"

defaultPassword :: String
defaultPassword = "test_auth_token"

defaultHost :: String
defaultHost = "localhost"

defaultPort :: String
defaultPort = "7687"

loadConfig :: IO Config
loadConfig = do
  Dotenv.loadFile Dotenv.defaultConfig
  maybeUser <- lookupEnv "NEO4J_USER"
  maybePassword <- lookupEnv "NEO4J_PASSWORD"
  maybeHost <- lookupEnv "NEO4J_HOST"
  maybePort <- lookupEnv "NEO4J_PORT"
  let user = fromMaybe defaultUser maybeUser
  let password = fromMaybe defaultPassword maybePassword
  let host = fromMaybe defaultHost maybeHost
  let port = read $ fromMaybe defaultPort maybePort
  unless
    (isJust maybeUser)
    (putStrLn "Warning: NEO4J_USER not set. Using default value.")
  unless
    (isJust maybePassword)
    (putStrLn "Warning: NEO4J_PASSWORD not set. Using default value.")
  unless
    (isJust maybeHost)
    (putStrLn "Warning: NEO4J_HOST not set. Using default value.")
  unless
    (isJust maybePort)
    (putStrLn "Warning: NEO4J_PORT not set. Using default value.")
  return $
    Config
      { dbUser = pack user
      , dbPassword = pack password
      , dbHost = pack host
      , dbPort = port
      }
