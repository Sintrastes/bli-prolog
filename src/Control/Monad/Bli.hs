
module Control.Monad.Bli(
  Bli,
  -- Basic interface
  runBli,
  runBliWithStore,
  initBli,
  getStore,
  setStore,
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


getStore = Generic.getStore @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

setStore = Generic.setStore @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

modifyStore = Generic.modifyStore @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

runBliWithStore = Generic.runBliWithStore @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

runBli = Generic.runBli @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

initBli = Generic.initBli @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

getConfig = Generic.getConfig  @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

getFacts = Generic.getFacts @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

getRelations = Generic.getRelations @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

getEntities = Generic.getEntities @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

getTypes = Generic.getTypes @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

getAliases = Generic.getAliases @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

-- | Attempts to add a new alias to the store. Returns a boolean flag to indicate success or failure.
newAlias = Generic.newAlias @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

-- | Attempts to add a new type to the store. Returns a boolean flag to indicate success or failure.
newType = Generic.newType @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

-- | Attempts to add a new entity to the store. Returns a boolean flag to indicate success or failure.
newEntity = Generic.newEntity @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure

newRelation = Generic.newRelation @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyConfig = Generic.modifyConfig  @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyFacts = Generic.modifyFacts  @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyRelations = Generic.modifyRelations @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyEntities = Generic.modifyEntities @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyTypes = Generic.modifyTypes @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setConfig = Generic.setConfig @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setFacts = Generic.setFacts @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setRelations = Generic.setRelations @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setEntities = Generic.setEntities @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setTypes = Generic.setTypes @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setAliases = Generic.setAliases @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
