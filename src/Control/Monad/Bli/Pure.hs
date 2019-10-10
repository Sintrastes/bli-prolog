
module Control.Monad.Bli.Pure where

--
-- | A pure version of the Bli monad. 
--   Can be used to favilitate the use of bli prolog
--   as an EDSL witihn Haskell
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.

import Data.Bli.Prolog.Ast
import Control.Applicative
import Control.Monad.State.Lazy
import Data.Schema
import Bli.App.Config (AppConfig)
import Control.Monad.Bli.Common
import qualified Control.Monad.Bli.Pure.Generic as Generic
import qualified Control.Monad.Bli as Bli

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

runBli = Generic.runBli  @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
getConfig = Generic.getConfig @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
getFacts = Generic.getFacts @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
getRelations = Generic.getRelations @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
getEntities = Generic.getEntities @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
getTypes = Generic.getTypes @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
getAliases = Generic.getAliases @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
newAlias = Generic.newAlias @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
newType = Generic.newType @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
newEntity = Generic.newEntity @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
newRelation = Generic.newRelation @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyConfig = Generic.modifyConfig @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyFacts = Generic.modifyFacts @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyRelations = Generic.modifyRelations @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyEntities = Generic.modifyEntities @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
modifyTypes = Generic.modifyTypes @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setConfig = Generic.setConfig @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setFacts = Generic.setFacts @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setRelations = Generic.setRelations @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setEntities = Generic.setEntities @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setTypes = Generic.setTypes @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
setAliases = Generic.setAliases @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
liftFromPure = Generic.liftFromPure  @FactContainer @RelationContainer @EntityContainer @TypeContainer @AliasDatastructure
