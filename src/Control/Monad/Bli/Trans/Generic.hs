
--
-- | Generic transformer version of the bli monad.
--

module Control.Monad.Bli.Trans.Generic (
  BliT(..),
  NewAliasResult(..),
  runBli,
  initBli,
  newTypes,
  newFacts,
  newEntities,
  newRelations,
  lookupTypeOfDataConstr,
  newDataType,
  newConstr,
  newConstrs,
  runBliWithStore,
  newScopedFact,
  newScopedFacts,
  clearScope,
  setStore,
  getConfig,
  getFacts,
  getScopedFacts,
  getEntitiesOfType,
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

import Prelude hiding (lookup, foldr, filter)
import Data.Foldable
import Data.Witherable
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Bli.App.Config
import Data.Alias
import Data.BliSet
import Control.Monad.Bli.Common
import Control.Empty
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Lens

-- For dealing with user-defined "proc" types.
import Data.Dynamic
import Type.Reflection

type BliT t1 t2 t3 t4 alias m a = 
 StateT (BliStore t1 t2 t3 t4 alias) m a

getStore :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliT t1 t2 t3 t4 alias m (BliStore t1 t2 t3 t4 alias)
getStore = get

modifyStore :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => ((BliStore t1 t2 t3 t4 alias) -> (BliStore t1 t2 t3 t4 alias)) -> BliT t1 t2 t3 t4 alias m ()
modifyStore = modify

setStore :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (BliStore t1 t2 t3 t4 alias) -> BliT t1 t2 t3 t4 alias m ()
setStore store = modify (\x -> store)

-- | Returns all of the entities of a given type.
getEntitiesOfType :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> BliT t1 t2 t3 t4 alias m (t3 String)
getEntitiesOfType typ = do
  entities <- getEntities
  return $ fmap fst $ filter (\x -> snd x == typ) entities

-- | Attempts to add a new fact to the store. Returns a boolean flag to indicate success or failure.
newFact :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Clause -> BliT t1 t2 t3 t4 alias m Bool
newFact clause = do
  facts <- getFacts
  case tryInsert clause facts of
    Left _ -> return False
    Right result -> do
      setFacts result
      return True

-- | Attempts to add a collection of facts to the store. Returns a boolean flag to indicate success or failure.
newFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => [Clause] -> BliT t1 t2 t3 t4 alias m Bool
newFacts clauses = do
  results <- mapM newFact clauses
  return $ foldr (&&) True results

-- | Tries to remove the given scope from the scoped facts. Returns
--   a boolean flag to indicate success or failure.
clearScope :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> BliT t1 t2 t3 t4 alias m Bool
clearScope scope = do
  scopedFacts <- getScopedFacts
  case Map.lookup scope scopedFacts of
    Just _  -> do
      modifyScopedFacts (\x -> Map.delete scope x)
      return True
    Nothing -> return $ False

-- | Attempts to add a new fact to the given scope. Returns a boolean flag to indicate success or failure.
newScopedFact :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Clause -> String -> BliT t1 t2 t3 t4 alias m Bool
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


-- | Attempts to add a collection of new facts to the given scope. Returns a boolean flag to indicate success or failure.
newScopedFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t1 Clause -> String -> BliT t1 t2 t3 t4 alias m Bool
newScopedFacts clauses scope = do
  results <- mapM (\x -> newScopedFact x scope) clauses
  return $ foldr (&&) True results

-- | Checks to see if an identifier is a primary ID
isPrimaryID :: (Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> BliT t1 t2 t3 t2 alias m Bool
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

lookupPrimaryID :: (Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> BliT t1 t2 t3 t2 alias m (Maybe String)
lookupPrimaryID id = do
  types <- getTypes
  relations <- getRelations
  entities <- getEntities
  aliases <- getAliases
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
newAlias :: (Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> String -> BliT t1 t2 t3 t2 alias m NewAliasResult
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
newType :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String ->  BliT t1 t2 t3 t4 alias m Bool
newType typeName = do
  types <- getTypes
  case tryInsert typeName types of
    Left  _ -> return False
    Right result -> do
      setTypes result
      return True

-- | Attempts to add a collection of types to the store. Returns a boolean flag to indicate success or failure.
newTypes :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => [String] ->  BliT t1 t2 t3 t4 alias m Bool
newTypes typeNames = do
  results <- mapM newType typeNames 
  return $ foldr (&&) True results

-- | Attempts to add a new entity to the store. Returns a boolean flag to indicate success or failure.
newEntity :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> String -> BliT t1 t2 t3 t4 alias m Bool
newEntity name entityType = do
  entities <- getEntities
  case tryInsert (name, entityType) entities of
    Left _       -> return False
    Right result -> do
      setEntities result
      return True

-- | Attempts to add a collection of entities to the store. Returns a boolean flag to indicate success or failure.
newEntities :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => [(String, String)] -> BliT t1 t2 t3 t4 alias m Bool
newEntities entityPairs = do
  results <- mapM (uncurry newEntity) entityPairs 
  return $ foldr (&&) True results

-- | Attempts to add a new relation to the store. Returns a boolean flag to indicate success or failure.
newRelation :: (Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> [String] -> BliT t1 t2 t3 t2 alias m Bool
newRelation name argumentTypes = do
  relns <- getRelations
  case tryInsert (name, argumentTypes) relns of
    Left result -> return False
    Right result -> do
      setRelations result
      return True

-- | Attempts to add a new relation to the store. Returns a boolean flag to indicate success or failure.
newRelations :: (Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => [(String, [String])] -> BliT t1 t2 t3 t2 alias m Bool
newRelations relDecs = do
  results <- mapM (uncurry newRelation) relDecs
  return $ foldr (&&) True results

-- | Builtin types.
initialTypes :: BliSet t4 => t4 TypeDecl
initialTypes = 
  let Right result = 
        tryInsert "string" empty >>=
        tryInsert "date" >>= 
        tryInsert "time" >>=
        tryInsert "type" >>=
        tryInsert "entity" >>=
        tryInsert "proc" >>=
        tryInsert "period" >>=
        tryInsert "rule"
  in result

-- | Builtin procs.
initialProcs :: BliSet t2 => t2 ProcContainer
initialProcs = 
  let Right result =
        tryInsert (ProcContainer ("putStrLn",
                                  [StringLitT],
                                  toDyn (putStrLn :: String -> IO ())))
                   empty
  in result

-- | Run a Bli computation with some initial application configuration data.
initBli :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => AppConfig -> BliT t1 t2 t3 t4 alias m a -> m a
initBli config app = evalStateT app 
  (BliStore {
    config = config,
    facts  = empty,
    scopedFacts = empty,
    relations = empty,
    entities = empty,
    types = initialTypes,
    dataTypes = empty,
    dataTypeConstrs = empty,
    procs = initialProcs,
    aliases = empty
  })
-- | Run a Bli application with some initial state.
runBli :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => AppConfig
  -> t1 Clause
  -> t2 RelDecl
  -> t3 EntityDecl
  -> t4 TypeDecl
  -> alias String
  -> BliT t1 t2 t3 t4 alias m a
  -> m a
runBli config facts relns ents types aliases app =
  evalStateT app (BliStore {
    config = config,
    facts  = facts,
    scopedFacts = empty,
    relations = relns,
    entities = empty,
    types = types, -- `union` initialTypes,
    dataTypes = empty,
    dataTypeConstrs = empty,
    procs = empty,
    aliases = aliases
  })

-- | Adds a new constructor to the store if it's type
--   has already been declared as a datatype, and all of its
--   arguments are valid types.
--   Returns a boolean result to indicate success/failure.
newConstr :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
-- | Constructor declaration
 => (String, [String])
-- | Type of constructor
 -> String
 -> BliT t1 t2 t3 t4 alias m Bool
newConstr constr typeName = do
  dataTypes <- dataTypes <$> get
  dataConstrs <- dataTypeConstrs <$> get
  -- If type has been declared as a datatype...
  case lookup (==typeName) dataTypes of
    Nothing -> return $ False
    Just _  -> do 
       case tryInsert (constr, typeName) dataConstrs of
         Left _ -> return $ False
         Right result -> do
           setDataConstrs result
           return $ True
           
setDataConstrs :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 ((String, [String]), String) -> BliT t1 t2 t3 t4 alias m ()
setDataConstrs val = modify (\bliCtx -> bliCtx { dataTypeConstrs = val } )

-- | Attemps to add a collection of new constructors to the store if everything
--   typechecks. Returns a boolean result to indicate success/failure.
newConstrs :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 -- Constructor name, constructor argument types, constructor type. 
 => [((String, [String]), String)] 
 -> BliT t1 t2 t3 t4 alias m Bool
newConstrs constrDecs = do
  results <- mapM (uncurry newConstr) constrDecs
  return $ foldr (&&) True results

-- | Helper function to add a new datatype to the store.
--   Returns "True" on success, False otherwise.
newDataType :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => DataTypeDecl -> BliT t1 t2 t3 t4 alias m Bool
newDataType (typeName, constrs) = do
  -- Note: I want this to short-circuit if there are errors ANYWHERE here,
  -- to ensure data integrity.
  dataTypes <- dataTypes <$> get
  case tryInsert typeName dataTypes of
    Left _ -> return False
    Right newDataTypes -> do
      setDataTypes newDataTypes
      newConstrs $ map (\constr -> 
                           (constr, typeName))
                        constrs
      
-- | Helper function to lookup the type of a data constructor
--   from the store.
--   Note: If we allow for ambigious data constructors, this should return a list.
lookupTypeOfDataConstr :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> BliT t1 t2 t3 t4 alias m (Maybe String)
lookupTypeOfDataConstr x = do
  dataTypeConstrs <- dataTypeConstrs <$> get
  case lookup (\((constrName,_),_) -> x==constrName) dataTypeConstrs of
    Just (_,typ) -> return $ Just typ
    Nothing -> return $ Nothing

-- | 
runBliWithStore :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliStore t1 t2 t3 t4 alias -> BliT t1 t2 t3 t4 alias m a -> m a
runBliWithStore store app = evalStateT app store

-- | Get the configuration data from a running bli application
getConfig :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => BliT t1 t2 t3 t4 alias m AppConfig
getConfig = config <$> get

-- | Get the currently stored facts from a running bli application.
getFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => BliT t1 t2 t3 t4 alias m (t1 Clause)
getFacts = do 
  facts <- (facts <$> get)
  -- Include any other facts currently in scope.
  scopedFacts <- (foldr (union) empty <$> scopedFacts <$> get)
  return $ facts `union` scopedFacts

getScopedFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => BliT t1 t2 t3 t4 alias m (Map String (t1 Clause))
getScopedFacts = (scopedFacts <$> get)

-- | Get the schema from a running bli application.
getRelations :: (Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => BliT t1 t2 t3 t2 alias m (t2 RelDecl)
getRelations = do
  standardRelations <- relations <$> get
  let Right singleton = tryInsert "entity" empty
  typePredicates    <- fmap (\typ -> (typ, singleton)) <$> types <$> get
  return $ standardRelations `union` typePredicates

getEntities :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliT t1 t2 t3 t4 alias m (t3 EntityDecl)
getEntities = entities <$> get

getTypes :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliT t1 t2 t3 t4 alias m (t4 TypeDecl)
getTypes = types <$> get

getAliases :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliT t1 t2 t3 t4 alias m (alias String)
getAliases = aliases <$> get

-- | Modify the configuration of a running bli application. 
modifyConfig :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (AppConfig -> AppConfig) -> BliT t1 t2 t3 t4 alias m ()
modifyConfig f = modify (\bliCtx -> bliCtx { config = f (config bliCtx) } )

-- | Modify the facts of a running bli application.
modifyFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t1 Clause -> t1 Clause) -> BliT t1 t2 t3 t4 alias m ()
modifyFacts f = modify (\bliCtx -> bliCtx { facts = f (facts bliCtx) } )

modifyScopedFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (Map String (t1 Clause) -> Map String (t1 Clause)) -> BliT t1 t2 t3 t4 alias m ()
modifyScopedFacts f = modify (\bliCtx -> bliCtx { scopedFacts = f (scopedFacts bliCtx) } )

-- | Modify the schema of a running bli application.
modifyRelations :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias) 
 => (t2 RelDecl -> t2 RelDecl) -> BliT t1 t2 t3 t4 alias m ()
modifyRelations f = modify (\bliCtx -> bliCtx { relations = f (relations bliCtx) } )

modifyEntities :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t3 EntityDecl -> t3 EntityDecl) -> BliT t1 t2 t3 t4 alias m ()
modifyEntities f = modify (\bliCtx -> bliCtx { entities = f (entities bliCtx) } )

modifyTypes :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (t4 TypeDecl -> t4 TypeDecl) -> BliT t1 t2 t3 t4 alias m ()
modifyTypes f = modify (\bliCtx -> bliCtx { types = f (types bliCtx) } )

modifyAliases :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => (alias String -> alias String) -> BliT t1 t2 t3 t4 alias m ()
modifyAliases f = modify (\bliCtx -> bliCtx { aliases = f (aliases bliCtx) } )

-- | Set the facts of a running bli application.
setFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t1 Clause -> BliT t1 t2 t3 t4 alias m ()
setFacts val = modify (\bliCtx -> bliCtx { facts = val } )

-- | Set the facts of a running bli application.
setScopedFacts :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Map String (t1 Clause) -> BliT t1 t2 t3 t4 alias m ()
setScopedFacts val = modify (\bliCtx -> bliCtx { scopedFacts = val } )

-- | Set the config of a running bli application
setConfig :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => AppConfig -> BliT t1 t2 t3 t4 alias m ()
setConfig val = modify (\bliCtx -> bliCtx { config = val } )

-- | Set the relations of a running bli application
setRelations :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 RelDecl -> BliT t1 t2 t3 t4 alias m ()
setRelations val = modify (\bliCtx -> bliCtx { relations = val } )

-- | Set the relations of a running bli application
setEntities :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t3 EntityDecl -> BliT t1 t2 t3 t4 alias m ()
setEntities val = modify (\bliCtx -> bliCtx { entities = val } )

-- | Set the relations of a running bli application
setTypes :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t4 TypeDecl -> BliT t1 t2 t3 t4 alias m ()
setTypes val = modify (\bliCtx -> bliCtx { types = val } )

-- | Set the relations of a running bli application
setDataTypes :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t2 String -> BliT t1 t2 t3 t4 alias m ()
setDataTypes val = modify (\bliCtx -> bliCtx { dataTypes = val } )

setAliases :: (Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => alias String -> BliT t1 t2 t3 t4 alias m ()
setAliases val = modify (\bliCtx -> bliCtx { aliases = val } )
