{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Infrastructure.Type
  ( ValueInteractants(..)
  , ParsingError(..)
  , convert
  , exact
  ) where

import           Control.Exception    (Exception)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Map.Strict      (Map, (!?))
import           Data.Text            (Text, unpack)
import           Database.Bolt        (Node (..), Value (..))
import           Models               (Catalyst (..), Molecule (..),
                                       RawReactionDetails (..), Reaction (..),
                                       ReactionDetails (..))
import           Prelude              hiding (id)

newtype ParsingError =
  ParsingError Text

instance Show ParsingError where
  show :: ParsingError -> String
  show (ParsingError msg) = "Interactants parsing error: " <> show msg

instance Exception ParsingError

class ValueInteractants a where
  exactEither :: Node -> Either ParsingError a

exact :: (MonadError ParsingError m, ValueInteractants a) => Node -> m a
exact = either throwError pure . exactEither

instance ValueInteractants Molecule where
  exactEither :: Node -> Either ParsingError Molecule
  exactEither Node {labels, nodeProps}
    | "Molecule" `elem` labels = do
      id <- int "id" nodeProps
      smiles <- string "smiles" nodeProps
      iupacName <- string "iupacName" nodeProps
      return Molecule {id, smiles, iupacName}
    | otherwise = Left $ ParsingError "No 'Molecule' label"

instance ValueInteractants Catalyst where
  exactEither :: Node -> Either ParsingError Catalyst
  exactEither Node {labels, nodeProps}
    | "Catalyst" `elem` labels = do
      id <- int "id" nodeProps
      smiles <- string "smiles" nodeProps
      name <- string "name" nodeProps
      return Catalyst {id, smiles, name}
    | otherwise = Left $ ParsingError "No 'Catalyst' label"

instance ValueInteractants Reaction where
  exactEither :: Node -> Either ParsingError Reaction
  exactEither Node {labels, nodeProps}
    | "Reaction" `elem` labels = do
      id <- int "id" nodeProps
      name <- string "name" nodeProps
      return Reaction {id, name}
    | otherwise = Left $ ParsingError "No 'Reaction' label"

int :: Text -> Map Text Value -> Either ParsingError Int
int key props =
  case props !? key of
    Just (I i) -> Right i
    _          -> (Left . ParsingError) ("No key: " <> key)

string :: Text -> Map Text Value -> Either ParsingError String
string key props =
  case props !? key of
    Just (T t) -> Right $ unpack t
    _          -> (Left . ParsingError) ("No key: " <> key)

convert :: RawReactionDetails -> ReactionDetails
convert RawDetails { reaction
                   , reagents
                   , products
                   , inbound
                   , outbound
                   , accelerate
                   , catalysts
                   } =
  Details
                              -- reaction = exact reaction
    {}
