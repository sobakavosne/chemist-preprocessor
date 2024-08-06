module Main where

import           API.Server           (startServer)
import qualified Configuration.Dotenv as Dotenv

main :: IO ()
main = do
  Dotenv.loadFile Dotenv.defaultConfig
  startServer
