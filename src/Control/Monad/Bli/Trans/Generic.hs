
--
-- | Generic transformer version of the bli monad.
--

module Control.Monad.Bli.Trans.Generic (
  BliT(..),
  StateBliT(..),
  MVarBliT(..),
  IORefBliT(..),
  NewAliasResult(..),
  -- High-level interface
  runBli,
  initBli,
  getProcs,
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
  newAlias,
  newType,
  newEntity,
  newRelation,
  newFact,
  -- Low level interface
  module Control.Monad.Bli.Trans.Generic.Internal
  --modifyConfig,
  --modifyFacts,
  --modifyScopedFacts,
  --modifyRelations,
  --modifyEntities,
  --modifyTypes,
  --setConfig,
  --setFacts,
  --setScopedFacts,
  --setRelations,
  --setEntities,
  --setTypes,
  --setAliases,
  --getStore,
  --modifyStore
  ) where
-- | Generic version of the Bli monad

import Prelude hiding (lookup, foldr, filter)
import Data.Foldable
import Data.Witherable
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Types
import Data.Bli.Prolog.Schema
import Bli.App.Config.Data
import Data.Alias
import Data.BliSet
import Control.Monad.Bli.Common
import Control.Empty
import Data.Map (Map)
import qualified Data.Map as Map
import System.Console.Haskeline.MonadException
import Control.Lens
import Data.Map.Lens
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Empty
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Compose
import Control.Monad.Trans.IO
import Data.IORef

-- For dealing with user-defined "proc" types.
import Data.Dynamic
import Type.Reflection

import Control.Monad.Bli.Trans.Generic.Internal

instance MonadException m => MonadException (IOT m) where
    controlIO f = fromIO $ join $ runIOT <$> f (RunIO (return))

instance (Monad (t2 m), MonadException (t (t2 m)), MonadIO (t (t2 m)), Monad m) => MonadException (ComposeT t t2 m) where
    controlIO f = do 
        ComposeT x <- liftIO $ f (RunIO return)
        ComposeT x

-- Note: To get this to work, we need this to be a newtype.
deriving instance MonadException m => MonadException (StateBliT t1 t2 t3 t4 alias m)
deriving instance MonadException m => MonadException (MVarBliT t1 t2 t3 t4 alias m)
deriving instance MonadException m => MonadException (IORefBliT t1 t2 t3 t4 alias m)

newtype StateBliT t1 t2 t3 t4 alias m a =  StateBliT { runBliT :: StateT (BliStore t1 t2 t3 t4 alias) m a }

instance Monad m => BliWrapper StateBliT m where 
  getStore    = StateBliT $ get
  modifyStore f = StateBliT $ (modify f) 
  evalBliT (StateBliT app) initialState = evalStateT app initialState

newtype MVarBliT t1 t2 t3 t4 alias m a = MVarBliT { runMVarBliT :: (ComposeT IOT (ReaderT (MVar (BliStore t1 t2 t3 t4 alias))) ) m a }

newtype IORefBliT t1 t2 t3 t4 alias m a = IORefBliT { runIORefBliT :: (ComposeT IOT (ReaderT (IORef (BliStore t1 t2 t3 t4 alias))) ) m a }

instance MonadIO m => BliWrapper MVarBliT m where
  getStore      =  MVarBliT $ ComposeT $ sequenceIO $ takeMVar <$> ask
  modifyStore f =  MVarBliT $ ComposeT $ sequenceIO $ ((\var -> modifyMVar_ var (return . f)) <$> ask)
  evalBliT (MVarBliT (ComposeT app)) state = do
               mvar <- liftIO $ newMVar state
               app' <- liftIO $ runIOT app
               runReaderT app' mvar

instance MonadIO m => BliWrapper IORefBliT m where
  getStore      = IORefBliT $ ComposeT $ sequenceIO $ readIORef <$> ask
  modifyStore f = IORefBliT $ ComposeT $ sequenceIO $ ((\var -> modifyIORef var f) <$> ask)
  evalBliT (IORefBliT (ComposeT app)) state = do
               ioRef <- liftIO $ newIORef state
               app' <- liftIO $ runIOT app
               runReaderT app' ioRef

deriving instance Functor m => Functor (MVarBliT t1 t2 t3 t4 alias m)
deriving instance Functor m => Functor (StateBliT t1 t2 t3 t4 alias m)
deriving instance Functor m => Functor (IORefBliT t1 t2 t3 t4 alias m)

deriving instance Monad m => Applicative (MVarBliT t1 t2 t3 t4 alias m)
deriving instance Monad m => Applicative (StateBliT t1 t2 t3 t4 alias m)
deriving instance Monad m => Applicative (IORefBliT t1 t2 t3 t4 alias m)

deriving instance Monad m => Monad (MVarBliT t1 t2 t3 t4 alias m)
deriving instance Monad m => Monad (StateBliT t1 t2 t3 t4 alias m)
deriving instance Monad m => Monad (IORefBliT t1 t2 t3 t4 alias m)

deriving instance (MonadIO m) => MonadIO (MVarBliT t1 t2 t3 t4 alias m)
deriving instance (MonadIO m) => MonadIO (StateBliT t1 t2 t3 t4 alias m)
deriving instance (MonadIO m) => MonadIO (IORefBliT t1 t2 t3 t4 alias m)

type BliT = StateBliT

-- | Returns all of the entities of a given type.
getEntitiesOfType :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> t t1 t2 t3 t4 alias m (t3 String)
getEntitiesOfType typ = do
  entities <- getEntities
  return $ fmap fst $ filter (\x -> snd x == typ) entities

-- | Attempts to add a new fact to the store. Returns a boolean flag to indicate success or failure.
newFact :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Clause -> t t1 t2 t3 t4 alias m Bool
newFact clause = do
  facts <- getFacts
  case tryInsert clause facts of
    Left _ -> return False
    Right result -> do
      setFacts result
      return True

-- | Attempts to add a collection of facts to the store. Returns a boolean flag to indicate success or failure.
newFacts :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => [Clause] -> t t1 t2 t3 t4 alias m Bool
newFacts clauses = do
  results <- mapM newFact clauses
  return $ foldr (&&) True results

-- | Tries to remove the given scope from the scoped facts. Returns
--   a boolean flag to indicate success or failure.
clearScope :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> t t1 t2 t3 t4 alias m Bool
clearScope scope = do
  scopedFacts <- getScopedFacts
  case Map.lookup scope scopedFacts of
    Just _  -> do
      modifyScopedFacts (\x -> Map.delete scope x)
      return True
    Nothing -> return $ False

-- | Attempts to add a new fact to the given scope. Returns a boolean flag to indicate success or failure.
newScopedFact :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => Clause -> String -> t t1 t2 t3 t4 alias m Bool
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
newScopedFacts :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => t1 Clause -> String -> t t1 t2 t3 t4 alias m Bool
newScopedFacts clauses scope = do
  results <- mapM (\x -> newScopedFact x scope) clauses
  return $ foldr (&&) True results

-- | Checks to see if an identifier is a primary ID
isPrimaryID :: (BliWrapper t m, Monad (t t1 t2 t3 t2 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> t t1 t2 t3 t2 alias m Bool
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

lookupPrimaryID :: (BliWrapper t m, Monad (t t1 t2 t3 t2 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> t t1 t2 t3 t2 alias m (Maybe String)
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
newAlias :: (BliWrapper t m, Monad (t t1 t2 t3 t2 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> String -> t t1 t2 t3 t2 alias m NewAliasResult
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
newType :: (BliWrapper t m, Monad (t t1 t2 t3 t2 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String ->  t t1 t2 t3 t2 alias m Bool
newType typeName = do
  types <- getTypes
  case tryInsert typeName types of
    Left  _ -> return False
    Right result -> do
      setTypes result
      return True

-- | Attempts to add a collection of types to the store. Returns a boolean flag to indicate success or failure.
newTypes :: (BliWrapper t m, Monad (t t1 t2 t3 t2 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => [String] ->  t t1 t2 t3 t2 alias m Bool
newTypes typeNames = do
  results <- mapM newType typeNames 
  return $ foldr (&&) True results

-- | Attempts to add a new entity to the store. Returns a boolean flag to indicate success or failure.
newEntity :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> String -> t t1 t2 t3 t4 alias m Bool
newEntity name entityType = do
  entities <- getEntities
  case tryInsert (name, entityType) entities of
    Left _       -> return False
    Right result -> do
      setEntities result
      return True

-- | Attempts to add a collection of entities to the store. Returns a boolean flag to indicate success or failure.
newEntities :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => [(String, String)] -> t t1 t2 t3 t4 alias m Bool
newEntities entityPairs = do
  results <- mapM (uncurry newEntity) entityPairs 
  return $ foldr (&&) True results

-- | Attempts to add a new relation to the store. Returns a boolean flag to indicate success or failure.
newRelation :: (BliWrapper t m, Monad m, Monad (t t1 t2 t3 t2 alias m), BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => String -> [String] -> t t1 t2 t3 t2 alias m Bool
newRelation name argumentTypes = do
  relns <- getRelations
  case tryInsert (name, argumentTypes) relns of
    Left result -> return False
    Right result -> do
      setRelations result
      return True

-- | Attempts to add a new relation to the store. Returns a boolean flag to indicate success or failure.
newRelations :: (BliWrapper t m, Monad (t t1 t2 t3 t2 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, Alias alias)
 => [(String, [String])] -> t t1 t2 t3 t2 alias m Bool
newRelations relDecs = do
  results <- mapM (uncurry newRelation) relDecs
  return $ foldr (&&) True results

-- | Builtin types.
initialTypes :: BliSet t4 => t4 TypeDecl
initialTypes = 
  let Right result = 
        tryInsert "string" empty >>=
        tryInsert "int" >>=
        tryInsert "float" >>=
        tryInsert "date" >>= 
        tryInsert "time" >>=
        tryInsert "type" >>=
        tryInsert "entity" >>=
        tryInsert "proc" >>=
        tryInsert "pred" >>=
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
initBli :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => AppConfig -> t t1 t2 t3 t4 alias m a -> m a
initBli config app = evalBliT app 
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
runBli :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => AppConfig
  -> t1 Clause
  -> t2 RelDecl
  -> t3 EntityDecl
  -> t4 TypeDecl
  -> alias String
  -> t t1 t2 t3 t4 alias m a
  -> m a
runBli config facts relns ents types aliases app =
  evalBliT app (BliStore {
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
newConstr :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
-- | Constructor declaration
 => (String, [String])
-- | Type of constructor
 -> String
 -> t t1 t2 t3 t4 alias m Bool
newConstr constr typeName = do
  dataTypes <- dataTypes <$> getStore
  dataConstrs <- dataTypeConstrs <$> getStore
  -- If type has been declared as a datatype...
  case lookup (==typeName) dataTypes of
    Nothing -> return $ False
    Just _  -> do 
       case tryInsert (constr, typeName) dataConstrs of
         Left _ -> return $ False
         Right result -> do
           setDataConstrs result
           return $ True

-- | Attemps to add a collection of new constructors to the store if everything
--   typechecks. Returns a boolean result to indicate success/failure.
newConstrs :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 -- Constructor name, constructor argument types, constructor type. 
 => [((String, [String]), String)] 
 -> t t1 t2 t3 t4 alias m Bool
newConstrs constrDecs = do
  results <- mapM (uncurry newConstr) constrDecs
  return $ foldr (&&) True results

-- | Helper function to add a new datatype to the store.
--   Returns "True" on success, False otherwise.
newDataType :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => DataTypeDecl -> t t1 t2 t3 t4 alias m Bool
newDataType (typeName, constrs) = do
  -- Note: I want this to short-circuit if there are errors ANYWHERE here,
  -- to ensure data integrity.
  dataTypes <- dataTypes <$> getStore
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
lookupTypeOfDataConstr :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => String -> t t1 t2 t3 t4 alias m (Maybe String)
lookupTypeOfDataConstr x = do
  dataTypeConstrs <- dataTypeConstrs <$> getStore
  case lookup (\((constrName,_),_) -> x==constrName) dataTypeConstrs of
    Just (_,typ) -> return $ Just typ
    Nothing -> return $ Nothing

runBliWithStore :: (BliWrapper t m, Monad (t t1 t2 t3 t4 alias m), Monad m, BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => BliStore t1 t2 t3 t4 alias -> t t1 t2 t3 t4 alias m a -> m a
runBliWithStore store app = evalBliT app store
