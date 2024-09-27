-- | This module defines the core data types and structures used throughout
--   the application for representing chemical reactions, mechanisms, and 
--   interactants. It includes various identifiers, relationship types, and 
--   masks for data handling, as well as details on the process and health checks.
--
--   ==== Exports
--
--   * `ReactionID`, `CatalystID`, `MoleculeID` - Unique identifiers for 
--     reactions, catalysts, and molecules.
--   * `ACCELERATE`, `PRODUCT_FROM`, `REAGENT_IN` - Relationship types that 
--     define connections between reactions and interactants.
--   * `Reaction`, `Catalyst`, `Molecule` - Data types representing different 
--     chemical entities and their relationships.
--   * `Interactant`, `Explain` - Types for handling interactants and mechanism 
--     explanations in the context of reactions.
--   * `NodeMask`, `PathMask`, `RawMechanismDetailsMask`, `RawReactionDetailsMask`, `RelMask` - 
--   WEB masks to abstract away direct types from the 
--   Hasbolt library for better type safety and ease of use.
--   * `ReactionDetails`, `MechanismDetails`, `ProcessDetails` - Data types 
--     that encapsulate detailed information about reactions and mechanisms.
--   * `ReactionNode`, `MechanismNode`, `RawReactionDetails`, `RawMechanismDetails` - Nodes representing the 
--     raw data structures in the graph database.
--   * `MechanismID`, `StageID` - Identifiers for mechanisms and stages within 
--     those mechanisms.
--   * `HealthCheck` - Type used for performing health checks on the database 
--     connection.
--
module Models
  ( ReactionID, CatalystID, MoleculeID, ACCELERATE(..), Catalyst(..), Molecule(..), PRODUCT_FROM(..), REAGENT_IN(..), Reaction(..)
  , Interactant(..), Explain(..)
  , NodeMask(..), PathMask(..), RawMechanismDetailsMask(..), RawReactionDetailsMask(..), RelMask(..)
  , ReactionDetails(..), MechanismDetails(..), ProcessDetails (..)
  , ReactionNode, MechanismNode, RawReactionDetails (..), RawMechanismDetails (..)
  , MechanismID, StageID, Mechanism (..), FOLLOW (..), Stage (..), INCLUDE (..)
  , HealthCheck (..)
  ) where

import           Models.Chemical
import           Models.Common
import           Models.Graph
import           Models.Interactant
import           Models.Mask
import           Models.Mechanism
import           Models.Process
