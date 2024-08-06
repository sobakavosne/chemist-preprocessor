{-# OPTIONS_GHC -Wno-name-shadowing #-}

module API.Server
  ( startServer
  ) where

import           API.Endpoints            (api, server)
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forever)
import           Data.Default             (Default (def))
import           Data.Maybe               (fromMaybe)
import           DataTypes                (ServerConfig (..))
import           Network.Wai.Handler.Warp (run)
import           Servant                  (serve)
import           System.Environment       (lookupEnv)

startServer :: IO ()
startServer = do
  port <- read . fromMaybe (show $ port def) <$> lookupEnv "SERVER_PORT"
  _ <-
    forkIO $ do
      putStrLn $ "Server is starting on port " ++ show port
      run port $ serve api server
  putStrLn $ "Server is operational on port " ++ show port
  forever $ threadDelay 60
