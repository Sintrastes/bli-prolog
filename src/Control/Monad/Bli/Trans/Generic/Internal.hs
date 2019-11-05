
--
-- | Internal (low-level) access to the resources
--   stored in the BliStore, encapsulated by the
--   BliT monad transformer.
--

module Control.Monad.Bli.Trans.Generic.Internal where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Compose
import Control.Monad.Trans.IO
import Data.Alias
import Data.BliSet
import Control.Monad.Bli.Common
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Types
import Data.Bli.Prolog.Schema
import Bli.App.Config.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Empty
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)

class Monad m => BliWrapper t m where
  getStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m)) 
    => t t1 t2 t3 t4 alias m (BliStore t1 t2 t3 t4 alias)
  modifyStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
    => ((BliStore t1 t2 t3 t4 alias) -> (BliStore t1 t2 t3 t4 alias)) -> t t1 t2 t3 t4 alias m ()
  evalBliT :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
    => t t1 t2 t3 t4 alias m a -> (BliStore t1 t2 t3 t4 alias) -> m a

  setStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (BliStore t1 t2 t3 t4 alias) -> t t1 t2 t3 t4 alias m ()
  setStore store = modifyStore (\x -> store)

  getProcs :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t t1 t2 t3 t4 alias m (t2 ProcContainer)
  getProcs = procs <$> getStore
  
  setDataConstrs :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t2 ((String, [String]), String) -> t t1 t2 t3 t4 alias m ()
  setDataConstrs val = modifyStore (\bliCtx -> bliCtx { dataTypeConstrs = val } )
  
  -- | Get the configuration data from a running bli application
  getConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
    => t t1 t2 t3 t4 alias m AppConfig
  getConfig = config <$> getStore
  
  -- | Get the currently stored facts from a running bli application.
  getFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
    => t t1 t2 t3 t4 alias m (t1 Clause)
  getFacts = do 
    facts <- (facts <$> getStore)
    -- Include any other facts currently in scope.
    scopedFacts <- (foldr (union) empty <$> scopedFacts <$> getStore)
    return $ facts `union` scopedFacts
  
  getScopedFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
    => t t1 t2 t3 t4 alias m (Map String (t1 Clause))
  getScopedFacts = (scopedFacts <$> getStore) 
  
  -- | Get the schema from a running bli application.
  getRelations :: (BliSet t1, BliSet t2, BliSet t3, Alias alias, Monad (t t1 t2 t3 t2 alias m))
   => t t1 t2 t3 t2 alias m (t2 RelDecl)
  getRelations = do
    standardRelations <- relations <$> getStore
    let Right singleton = tryInsert "entity" empty
    typePredicates    <- fmap (\typ -> (typ, singleton)) <$> types <$> getStore
    return $ standardRelations `union` typePredicates
  
  getEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t t1 t2 t3 t4 alias m (t3 EntityDecl)
  getEntities = entities <$> getStore
  
  getTypes :: (BliSet t1, BliSet t2, BliSet t3, Alias alias, Monad (t t1 t2 t3 t2 alias m))
   => t t1 t2 t3 t2 alias m (t2 TypeDecl)
  getTypes = do
    standardTypes <- types <$> getStore
    dataTypes <- dataTypes <$> getStore
    return $ standardTypes `union` dataTypes
  
  getAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t t1 t2 t3 t4 alias m (alias String)
  getAliases = aliases <$> getStore

  -- | Modify the configuration of a running bli application. 
  modifyConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (AppConfig -> AppConfig) -> t t1 t2 t3 t4 alias m ()
  modifyConfig f = modifyStore (\bliCtx -> bliCtx { config = f (config bliCtx) } )
  
  -- | Modify the facts of a running bli application.
  modifyFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (t1 Clause -> t1 Clause) -> t t1 t2 t3 t4 alias m ()
  modifyFacts f = modifyStore (\bliCtx -> bliCtx { facts = f (facts bliCtx) } )
  
  modifyScopedFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (Map String (t1 Clause) -> Map String (t1 Clause)) -> t t1 t2 t3 t4 alias m ()
  modifyScopedFacts f = modifyStore (\bliCtx -> bliCtx { scopedFacts = f (scopedFacts bliCtx) } ) 
  
  -- | Modify the schema of a running bli application.
  modifyRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m)) 
   => (t2 RelDecl -> t2 RelDecl) -> t t1 t2 t3 t4 alias m ()
  modifyRelations f = modifyStore (\bliCtx -> bliCtx { relations = f (relations bliCtx) } )

  modifyEntities :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (t3 EntityDecl -> t3 EntityDecl) -> t t1 t2 t3 t4 alias m ()
  modifyEntities f = modifyStore (\bliCtx -> bliCtx { entities = f (entities bliCtx) } )
  
  modifyTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (t4 TypeDecl -> t4 TypeDecl) -> t t1 t2 t3 t4 alias m ()
  modifyTypes f = modifyStore (\bliCtx -> bliCtx { types = f (types bliCtx) } )
  
  modifyAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => (alias String -> alias String) -> t t1 t2 t3 t4 alias m ()
  modifyAliases f = modifyStore (\bliCtx -> bliCtx { aliases = f (aliases bliCtx) } )
  
  -- | Set the facts of a running bli application.
  setFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t1 Clause -> t t1 t2 t3 t4 alias m ()
  setFacts val = modifyStore (\bliCtx -> bliCtx { facts = val } )

  -- | Set the facts of a running bli application.
  setScopedFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => Map String (t1 Clause) -> t t1 t2 t3 t4 alias m ()
  setScopedFacts val = modifyStore (\bliCtx -> bliCtx { scopedFacts = val } )
  
  -- | Set the config of a running bli application
  setConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => AppConfig -> t t1 t2 t3 t4 alias m ()
  setConfig val = modifyStore (\bliCtx -> bliCtx { config = val } )
  
  -- | Set the relations of a running bli application
  setRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t2 RelDecl -> t t1 t2 t3 t4 alias m ()
  setRelations val = modifyStore (\bliCtx -> bliCtx { relations = val } )
  
  -- | Set the relations of a running bli application
  setEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t3 EntityDecl -> t t1 t2 t3 t4 alias m ()
  setEntities val = modifyStore (\bliCtx -> bliCtx { entities = val } )
  
  -- | Set the relations of a running bli application
  setTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t4 TypeDecl -> t t1 t2 t3 t4 alias m ()
  setTypes val = modifyStore (\bliCtx -> bliCtx { types = val } )
  
  -- | Set the relations of a running bli application
  setDataTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => t2 String -> t t1 t2 t3 t4 alias m ()
  setDataTypes val = modifyStore (\bliCtx -> bliCtx { dataTypes = val } )
  
  setAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias, Monad (t t1 t2 t3 t4 alias m))
   => alias String -> t t1 t2 t3 t4 alias m ()
  setAliases val = modifyStore (\bliCtx -> bliCtx { aliases = val } )
