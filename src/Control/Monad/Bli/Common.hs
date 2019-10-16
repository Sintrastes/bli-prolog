
module Control.Monad.Bli.Common where

import Data.Alias
import Data.BliSet
import Data.Map (Map)
import Bli.App.Config
import Data.Bli.Prolog.Schema
import Data.Bli.Prolog.Ast

-- For dealing with user-defined "proc" types.
import Data.Dynamic
import Type.Reflection

newtype ProcContainer = ProcContainer (String, [BliPrologType], Dynamic)

instance Eq ProcContainer where
  -- Punt on the issue of comparing Dynamics
  (ProcContainer (x,y,z)) == (ProcContainer (x',y',z')) = (x,y) == (x',y')

instance Ord ProcContainer where
  -- Ignore the third component in our ord instance.
  -- I'm not sure if it's possible to derive Ord for Dynamic.
  (ProcContainer (x,y,z)) <= (ProcContainer (x',y',z')) = (x,y) <= (x',y')

data BliStore t1 t2 t3 t4 alias = BliStore {
  config          :: AppConfig,
  facts           :: t1 Clause,
  scopedFacts     :: Map String (t1 Clause),
  relations       :: t2 RelDecl,
  entities        :: t3 EntityDecl,
  types           :: t4 TypeDecl,
  -- Internal store, this should probably be merged with "types" when
  -- they are displayed to the user.
  dataTypes       :: t2 String,
  -- This is really a map, I should probably consider changing the type
  dataTypeConstrs :: t2 (String, String),
  -- | Name of the declared proc, a list of BliPrologTypes to use as
  -- argument to the procedure, and the procedure itself as a Dynamic
  -- value.
  procs       :: t2 ProcContainer,
  aliases     :: alias String
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
