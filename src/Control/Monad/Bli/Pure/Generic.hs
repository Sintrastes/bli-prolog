
module Control.Monad.Bli.Pure.Generic where

-- | Generic version of the pure Bli monad

import Data.Prolog.Ast
import Control.Applicative
import Data.Schema
import Data.BliSet
import Control.Monad.State.Lazy
import Control.Monad.Bli.Common
import Bli.App.Config (AppConfig)

-- For writing liftFromPure
import qualified Control.Monad.Bli.Generic as Bli

-- | A monad for wrapping pure computations done (and run) in bli prolog.
type Bli t1 t2 t3 t4 alias a = State BliStore a

-- | Run a Bli computation with some initial application configuration data.
initBli :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => AppConfig
initBli config = evalState (BliStore config empty empty empty empty) 

-- | Run a pure Bli computation with some initial state.
runBli :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => AppConfig -> t1 Clause -> t2 RelDecl -> t3 EntityDecl -> t4 TypeDecl -> alias -> Bli t1 t2 a -> a
runBli config facts relations entities types aliases app =
  evalState app (BliStore config facts relations entities types aliases)

-- | Attempts to add a new alias to the store. Returns a boolean flag to indicate success or failure.
newAlias :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => String -> String -> Bli t1 t2 t3 t4 alias Bool
newAlias id1 id2 = do
  aliases <- getAliases
  case addNewAlias' aliases (id1, id2) of
    Nothing -> return False
    Just result -> 
      setAliases result
      return True

-- | Attempts to add a new type to the store. Returns a boolean flag to indicate success or failure.
newType :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String ->  Bli t1 t2 t3 t4 alias Bool
newType typeName = do
  types <- getTypes
  case tryInsert typeName types of
    Left  _ -> return False
    Right result -> do
      setTypes result
      return True

-- | Attempts to add a new entity to the store. Returns a boolean flag to indicate success or failure.
newEntity :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> String -> Bli t1 t2 t3 t4 alias Bool
newEntity name entityType = do
  entities <- getEntities
  case tryInsert (name, entityType) entities of
    Left _       -> return False
    Right result -> do
      setEntities result
      return True

-- | Attempts to add a new relation to the store. Returns a boolean flag to indicate success or failure.
newRelation :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> [String] -> Bli t1 t2 t3 t4 alias Bool
newRelation name argumentTypes = do
  relns <- getRelations
  case tryInsert (name, argumentTypes) relns of
    Left result -> return False
    Right result -> do
      setRelations result
      return True

-- | Get the options from a pure bli computation.
getConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias AppConfig
getConfig = config <$> get

-- | Get the program from a pure bli computation.
getFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t1 Clause)
getFacts = facts <$> get

-- | Get the schema from a pure bli computation.
getRelations  :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t2 RelDecl)
getRelations = relations <$> get

-- | Get the schema from a pure bli computation.
getEntities  :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t2 EntityDecl)
getEntities = entities <$> get

-- | Get the schema from a pure bli computation.
getTypes  :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t2 TypeDecl)
getTypes = types <$> get

-- | Get the schema from a pure bli computation.
getAliases  :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias alias
getAliases = aliases <$> get

-- | Modify the configuration of a running bli application. 
modifyConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (AppConfig -> AppConfig) -> Bli t1 t2 t3 t4 alias ()
modifyConfig f = modify (\bliCtx -> bliCtx { config = f (config bliCtx) } )

-- | Modify the facts of a running bli application.
modifyFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t1 Clause -> t1 Clause) -> Bli t1 t2 t3 t4 alias ()
modifyFacts f = modify (\bliCtx -> bliCtx { facts = f (facts bliCtx) } )

-- | Modify the schema of a running bli application.
modifyRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t2 SchemaEntry -> t2 SchemaEntry) -> Bli t1 t2 t3 t4 alias ()
modifyRelations f = modify (\bliCtx -> bliCtx { relations = f (relations bliCtx) } )

modifyEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t3 RelDecl -> t3 RelDecl) -> Bli t1 t2 t3 t4 alias ()
modifyEntities f = modify (\bliCtx -> bliCtx { entities = f (entities bliCtx) } )

modifyTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t4 TypeDecl -> t4 TypeDecl) ->
modifyTypes = modify (\bliCtx -> bliCtx { types = f (types bliCtx) } )

modifyAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (alias -> alias) -> Bli t1 t2 t3 t4 t5 alias ()
modifyAliases = modify (\bliCtx -> bliCtx { aliases = f (aliases bliCtx) } )

-- | Set the facts of a running bli application.
setFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => t1 Clause -> Bli t1 t2 t3 t4 alias ()
setFacts val = modify (\bliCtx -> bliCtx { facts = val } )

-- | Set the config of a running bli application
setConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => AppConfig -> Bli t1 t2 t3 t4 alias ()
setConfig val = modify (\bliCtx -> bliCtx { config = val } )

-- | Set the relations of a running bli application
setRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 SchemaEntry -> Bli t1 t2 t3 t4 alias ()
setRelations val = modify (\bliCtx -> bliCtx { relations = val } )

-- | Set the relations of a running bli application
setEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 SchemaEntry -> Bli t1 t2 t3 t4 alias ()
setEntities val = modify (\bliCtx -> bliCtx { entities = val } )

-- | Set the relations of a running bli application
setTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 SchemaEntry -> Bli t1 t2 t3 t4 alias ()
setTypes val = modify (\bliCtx -> bliCtx { types = val } )

setAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => alias -> Bli t1 t2 t3 t4 alias ()
setTypes val = (\bliCtx -> bliCtx { aliases = val } )

-- Helper function to go from the pure to the impure version of the Bli monad.
liftFromPure :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t3 alias a -> Bli.Bli t1 t2 t3 t4 alias a
liftFromPure x = do
  store <- Bli.get
  let store' = execState x store
  Bli.modify (\_ -> store')
  return $ evalState x store
  