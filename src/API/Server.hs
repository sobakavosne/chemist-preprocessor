{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

module API.Server
  ( startServer
  ) where

import           API.Endpoints            (api, server)
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forever)
import           Data.Default             (Default (def))
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (run)
import           Servant                  (serve)
import           System.Environment       (lookupEnv)

data ServerConfig =
  ServerConfig
    { port :: Int
    , host :: String
    }
  deriving (Generic)

instance Default ServerConfig where
  def :: ServerConfig
  def = ServerConfig {port = 8080, host = "127.0.0.1"}

startServer :: IO ()
startServer = do
  port' <- read . fromMaybe (show $ port def) <$> lookupEnv "SERVER_PORT"
  _ <-
    forkIO $ do
      putStrLn $ "Server is starting on port " ++ show port'
      run port' $ serve api server
  putStrLn $ "Server is operational on port " ++ show port'
  forever $ threadDelay 60
