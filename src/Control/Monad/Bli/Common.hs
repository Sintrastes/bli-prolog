
module Control.Monad.Bli.Common where

import Data.Alias
import Data.BliSet
import Data.Map (Map)
import Bli.App.Config
import Data.Schema
import Data.Prolog.Ast

data BliStore t1 t2 t3 t4 alias = BliStore {
  config    :: AppConfig,
  facts     :: t1 Clause,
  relations :: t2 RelDecl,
  entities  :: t3 EntityDecl,
  types     :: t4 TypeDecl,
  aliases   :: alias String
}

-- Types to use for our containers

 -- | The container to use for the fact store
type FactContainer      = []
 -- | The container to use for the relational store 
type RelationContainer  = []
 -- | The container to use for the entity store
type EntityContainer    = []
 -- | The container to use for the type store
type TypeContainer      = []
 -- | The datastructure to use for storing aliases
type AliasDatastructure = Map String
