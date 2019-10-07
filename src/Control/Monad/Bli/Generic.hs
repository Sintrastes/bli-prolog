
module Control.Monad.Bli.Generic(
  runBli,
  getConfig,
  getFacts,
  getRelations,
  getEntities,
  getTypes,
  getAliases,
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

-- | Generic version of the Bli monad

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Prolog.Ast
import Data.Schema
import Bli.App.Config
import Data.BliSet

data BliStore t1 t2 t3 t4 alias = BliStore {
  config :: AppConfig,
  facts  :: t1 Clause,
  relations :: t2 RelDecl,
  entities :: t3 EntityDecl,
  types :: t4 TypeDecl,
  aliases :: alias
}

type Bli t1 t2 t3 t4 alias a = 
 StateT (BliStore t1 t2 t3 t4 alias) IO a

-- | Lift io computations into the Bli monad.
--   NOTE: We should just be able to use liftIO here instead, from the 
--         MonadIO class.
io :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
 => IO a 
 -> Bli t1 t2 t2 t3 t4 alias a
io = lift

-- | Run a Bli application with some initial state.
runBli :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => AppConfig
  -> t1 Clause
  -> t2 RelDecl
  -> t3 EntityDecl
  -> t4 TypeDecl
  -> alias
  -> Bli t1 t2 t3 t4 alias a
  -> IO a
runBli config facts relns ents types aliases app =
  evalStateT app (BliStore options facts relns ents types aliases)

-- | Get the configuration data from a running bli application
getConfig :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => Bli t1 t2 t3 t4 alias AppConfig
getConfig = config <$> get

-- | Get the currently stored facts from a running bli application.
getFacts :: (BliSet t1, BliSet t2, BliSet t3, BliSet t4, Alias alias)
  => Bli t1 t2 t3 t4 alias (t1 Clause)
getFacts = facts <$> get

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
 => Bli t1 t2 t3 t3 t4 alias (t4 TypeDecl)
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
