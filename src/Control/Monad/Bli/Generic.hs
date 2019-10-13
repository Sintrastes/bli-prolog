
module Control.Monad.Bli.Generic(
  -- Basic interface
  Bli(..),
  NewAliasResult(..),
  runBli,
  initBli,
  runBliWithStore,
  newScopedFact,
  clearScope,
  setStore,
  getConfig,
  getFacts,
  getScopedFacts,
  getRelations,
  getEntities,
  getTypes,
  getAliases,
  lookupPrimaryID,
  -- High-level interface
  newAlias,
  newType,
  newEntity,
  newRelation,
  newFact,
  -- Low level interface
  modifyConfig,
  modifyFacts,
  modifyScopedFacts,
  modifyRelations,
  modifyEntities,
  modifyTypes,
  setConfig,
  setFacts,
  setScopedFacts,
  setRelations,
  setEntities,
  setTypes,
  setAliases,
  getStore,
  modifyStore) where

-- | Generic version of the Bli monad

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Bli.Prolog.Ast
import Data.Schema
import Bli.App.Config
import Data.Alias
import Data.BliSet
import Control.Monad.Bli.Common
import Control.Empty
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Lens

type Bli t1 t2 t3 t4 alias a = 
 StateT (BliStore t1 t2 t3 t4 alias) IO a

getStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => Bli t1 t2 t3 t4 alias (BliStore t1 t2 t3 t4 alias)
getStore = get

modifyStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => ((BliStore t1 t2 t3 t4 alias) -> (BliStore t1 t2 t3 t4 alias)) -> Bli t1 t2 t3 t4 alias ()
modifyStore = modify

setStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => (BliStore t1 t2 t3 t4 alias) -> Bli t1 t2 t3 t4 alias ()
setStore store = modify (\x -> store)

-- | Attempts to add a new fact to the store. Returns a boolean flag to indicate success or failure.
newFact :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => Clause -> Bli t1 t2 t3 t4 alias Bool
newFact clause = do
  facts <- getFacts
  case tryInsert clause facts of
    Left _ -> return False
    Right result -> do
      setFacts result
      return True

-- | Tries to remove the given scope from the scoped facts. Returns
--   a boolean flag to indicate success or failure.
clearScope :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => String -> Bli t1 t2 t3 t4 alias Bool
clearScope scope = do
  scopedFacts <- getScopedFacts
  case Map.lookup scope scopedFacts of
    Just _  -> do
      modifyScopedFacts (\x -> Map.delete scope x)
      return True
    Nothing -> return $ False

-- | Attempts to add a new fact to the given scope. Returns a boolean flag to indicate success or failure.
newScopedFact :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => Clause -> String -> Bli t1 t2 t3 t4 alias Bool
newScopedFact clause scope = do
  scopedFacts <- getScopedFacts
  case scopedFacts ^.at scope of
    Just scopeFacts ->
      case tryInsert clause scopeFacts of
        Left _ -> return False
        Right result -> do
          setScopedFacts $ over (at scope) ((\_ -> result)<$>) scopedFacts
          return True
    Nothing -> do
      -- If scope doesn't exist, create it and try again.
      _ <- setScopedFacts (Map.insert scope empty scopedFacts)
      newScopedFact clause scope

-- | Checks to see if an identifier is a primary ID
isPrimaryID :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => String -> Bli t1 t2 t3 t4 alias Bool
isPrimaryID id = do
  types <- getTypes
  relations <- getRelations
  entities <- getEntities
  let result =
        msum [lookup (==id) types
             ,fst <$> lookup (\x ->(fst x)== id) relations
             ,fst <$> lookup (\x -> (fst x)==id) entities]
  case result of
    Just x  -> return $ True
    Nothing -> return $ False

lookupPrimaryID :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> Bli t1 t2 t3 t4 alias (Maybe String)
lookupPrimaryID id = do
  types <- getTypes
  relations <- getRelations
  entities <- getEntities
  aliases <- getAliases
  lift $ putStrLn "In lookupPrimaryID" -- debugging
  let result =
        msum [lookup (==id) types
             ,fst <$> lookup (\x ->(fst x)== id) relations
             ,fst <$> lookup (\x -> (fst x)==id) entities]
  case result of
    Just x  -> return $ Just $ x
    Nothing -> return $ getPID aliases id

data NewAliasResult = 
   SuccessfullyAdded
 | AliasAlreadyInStore
 | DoesNotHavePrimaryIDOrAlias

-- | Attempts to add a new alias to the store. Returns a boolean flag to indicate success or failure.
newAlias :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => String -> String -> Bli t1 t2 t3 t4 alias NewAliasResult
newAlias id1 id2 = do
  aliases <- getAliases
  let lookupResult = (getPID aliases id1 == getPID aliases id2) && (getPID aliases id1 /= Nothing)
  case lookupResult of
    True -> return AliasAlreadyInStore
    False -> do
      lookupResult <- lookupPrimaryID id1
      case lookupResult of
        Just primaryId1 -> do
          setAliases $ insertNewAlias' aliases (id2, primaryId1)
          return SuccessfullyAdded
        Nothing -> do 
          lookupResult <- lookupPrimaryID id2
          case lookupResult of
            Just primaryId2 -> do 
              setAliases $ insertNewAlias' aliases (id1, primaryId2)
              return SuccessfullyAdded
            Nothing -> return DoesNotHavePrimaryIDOrAlias

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

-- | Run a Bli computation with some initial application configuration data.
initBli :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => AppConfig -> Bli t1 t2 t3 t4 alias a -> IO a
initBli config app = evalStateT app (BliStore config empty empty empty empty empty empty) 

-- | Run a Bli application with some initial state.
runBli :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => AppConfig
  -> t1 Clause
  -> t2 RelDecl
  -> t3 EntityDecl
  -> t4 TypeDecl
  -> alias String
  -> Bli t1 t2 t3 t4 alias a
  -> IO a
runBli config facts relns ents types aliases app =
  evalStateT app (BliStore config facts empty relns ents types aliases)

-- | 
runBliWithStore :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliStore t1 t2 t3 t4 alias -> Bli t1 t2 t3 t4 alias a -> IO a
runBliWithStore store app = evalStateT app store

-- | Get the configuration data from a running bli application
getConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => Bli t1 t2 t3 t4 alias AppConfig
getConfig = config <$> get


-- | Get the currently stored facts from a running bli application.
getFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => Bli t1 t2 t3 t4 alias (t1 Clause)
getFacts = do 
  facts <- (facts <$> get)
  -- Include any other facts currently in scope.
  scopedFacts <- (foldr (union) empty <$> scopedFacts <$> get)
  return $ facts `union` scopedFacts


getScopedFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => Bli t1 t2 t3 t4 alias (Map String (t1 Clause))
getScopedFacts = (scopedFacts <$> get)

-- | Get the schema from a running bli application.
getRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t2 RelDecl)
getRelations = relations <$> get

getEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t3 EntityDecl)
getEntities = entities <$> get

getTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (t4 TypeDecl)
getTypes = types <$> get

getAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Bli t1 t2 t3 t4 alias (alias String)
getAliases = aliases <$> get

-- | Modify the configuration of a running bli application. 
modifyConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (AppConfig -> AppConfig) -> Bli t1 t2 t3 t4 alias ()
modifyConfig f = modify (\bliCtx -> bliCtx { config = f (config bliCtx) } )

-- | Modify the facts of a running bli application.
modifyFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t1 Clause -> t1 Clause) -> Bli t1 t2 t3 t4 alias ()
modifyFacts f = modify (\bliCtx -> bliCtx { facts = f (facts bliCtx) } )

modifyScopedFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (Map String (t1 Clause) -> Map String (t1 Clause)) -> Bli t1 t2 t3 t4 alias ()
modifyScopedFacts f = modify (\bliCtx -> bliCtx { scopedFacts = f (scopedFacts bliCtx) } )

-- | Modify the schema of a running bli application.
modifyRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t2 RelDecl -> t2 RelDecl) -> Bli t1 t2 t3 t4 alias ()
modifyRelations f = modify (\bliCtx -> bliCtx { relations = f (relations bliCtx) } )

modifyEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t3 EntityDecl -> t3 EntityDecl) -> Bli t1 t2 t3 t4 alias ()
modifyEntities f = modify (\bliCtx -> bliCtx { entities = f (entities bliCtx) } )

modifyTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t4 TypeDecl -> t4 TypeDecl) -> Bli t1 t2 t3 t4 alias ()
modifyTypes f = modify (\bliCtx -> bliCtx { types = f (types bliCtx) } )

modifyAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (alias String -> alias String) -> Bli t1 t2 t3 t4 alias ()
modifyAliases f = modify (\bliCtx -> bliCtx { aliases = f (aliases bliCtx) } )

-- | Set the facts of a running bli application.
setFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => t1 Clause -> Bli t1 t2 t3 t4 alias ()
setFacts val = modify (\bliCtx -> bliCtx { facts = val } )

-- | Set the facts of a running bli application.
setScopedFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => Map String (t1 Clause) -> Bli t1 t2 t3 t4 alias ()
setScopedFacts val = modify (\bliCtx -> bliCtx { scopedFacts = val } )

-- | Set the config of a running bli application
setConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => AppConfig -> Bli t1 t2 t3 t4 alias ()
setConfig val = modify (\bliCtx -> bliCtx { config = val } )

-- | Set the relations of a running bli application
setRelations :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 RelDecl -> Bli t1 t2 t3 t4 alias ()
setRelations val = modify (\bliCtx -> bliCtx { relations = val } )

-- | Set the relations of a running bli application
setEntities :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t3 EntityDecl -> Bli t1 t2 t3 t4 alias ()
setEntities val = modify (\bliCtx -> bliCtx { entities = val } )

-- | Set the relations of a running bli application
setTypes :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t4 TypeDecl -> Bli t1 t2 t3 t4 alias ()
setTypes val = modify (\bliCtx -> bliCtx { types = val } )

setAliases :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => alias String -> Bli t1 t2 t3 t4 alias ()
setAliases val = modify (\bliCtx -> bliCtx { aliases = val } )
