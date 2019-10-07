
module Control.Monad.Bli(
  -- Basic interface
  runBli,
  getConfig,
  getFacts,
  getRelations,
  getEntities,
  getTypes,
  getAliases,
  -- High-level interface
  newAlias,
  newType,
  newEntity,
  newRelation,
  -- Low level interface
  modifyConfig,
  modifyFacts,
  modifyRelations,
  modifyEntities,
  modifyTypes,
  setConfig,
  setFacts,
  setRelations,
  setEntities,
  setTypes,
  setAliases) where

--
-- | A monad for IO computations preformed in the context of a 
--   running bli prolog session.
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.
--

import Data.Prolog.Ast
import Data.Schema
import Bli.App.Config (AppConfig)
import qualified Control.Monad.Bli.Generic as Generic
import Control.Monad.Bli.Common

-- | A monad for wrapping computations done (and run) in bli prolog.
type Bli a = Generic.Bli 
 -- | The container to use for the fact store
    FactContainer
 -- | The container to use for the relational store 
    RelationContainer
 -- | The container to use for the entity store
    EntityContainer
 -- | The container to use for the type store
    TypeContainer
 -- | The datastructure to use for storing aliases
    AliasDatastructure 
    a

-- | Lift io computations into the Bli monad.
--   NOTE: This is depreciated
io :: IO a -> Bli a
io = Generic.io

runBli = Generic.runBli
getConfig = Generic.getConfig
getFacts = Generic.getFacts
getRelations = Generic.getRelations
getEntities = Generic.getEntities
getTypes = Generic.getTypes
getAliases = Generic.getAliases
newAlias = Generic.newAlias
newType = Generic.newType
newEntity = Generic.newEntity
newRelation = Generic.newRelation
modifyConfig = Generic.modifyConfig
modifyFacts = Generic.modifyFacts
modifyRelations = Generic.modifyRelations
modifyEntities = Generic.modifyEntities
modifyTypes = Generic.modifyTypes
setConfig = Generic.setConfig
setFacts = Generic.setFacts
setRelations = Generic.setRelations
setEntities = Generic.setEntities
setTypes = Generic.setTypes
setAliases = Generic.setAliases
