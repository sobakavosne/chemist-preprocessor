{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Data.Aeson               (FromJSON, ToJSON)

import           DataTypes                (Reaction)
import qualified DataTypes                as T
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant                  as S

type API
   = "addMolecule" S.:> S.ReqBody '[ S.JSON] T.Molecule S.:> S.Post '[ S.JSON] T.Molecule

api :: S.Proxy API
api = S.Proxy

addReactionHandler :: T.Reaction -> S.Handler Reaction
addReactionHandler = return

addMoleculeHandler :: T.Molecule -> S.Handler T.Molecule
addMoleculeHandler = return

server :: (T.Molecule -> S.Handler T.Molecule)
server = addMoleculeHandler

app :: Wai.Application
app = S.serve api server

main :: IO ()
main = Warp.run 8080 app
