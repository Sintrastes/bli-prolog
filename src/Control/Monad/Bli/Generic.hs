
module Control.Monad.Bli.Generic where

-- | Generic version of the Bli monad

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Prolog.Ast
import Data.Schema
import Bli.App.Config
import Data.BliSet

type Bli t1 t2 t3 t4 alias a = 
 StateT (
   -- Application configuration data
   AppConfig, 
   -- List of asserted predicates and rules
   t1 Clause, 
   -- List of all relations in the schema.
   t2 RelDecl,
   -- List of all terms in the schema.
   t3 EntityDecl,
   -- List of all types in the schema
   t4 TypeDecl,
   -- Interface for getting all type aliases. I'm not sure how this should be implemented,
   alias
 ) IO a

-- | Lift io computations into the Bli monad.
io :: (BliSet t1,
       BliSet t2,
       BliSet t3,
       BliSet t4,
       Alias alias)
 => IO a 
 -> Bli t1 t2 t2 t3 t4 alias a
io = lift



-- | Run a Bli application with some initial state.
runBli :: (BliSet t1,
           BliSet t2,
           BliSet t3,
           BliSet t4,
           Alias alias)
  => AppConfig
  -> t1 Clause
  -> t2 RelDecl
  -> t3 EntityDecl
  -> t4 TypeDecl
  -> alias
  -> Bli t1 t2 t3 t4 alias a
  -> IO a
runBli options program relns ents types aliases app =
  evalStateT app (options, program, relns, ents, types, aliases)

-- | Get the options from a running bli application
getOpts    :: (BliSet t1,
               BliSet t2,
               BliSet t3,
               BliSet t4,
               Alias alias)
 => Bli t1 t2 t3 t4 alias AppConfig
getOpts = (\(x,y,z,_,_,_) -> x) <$> get

-- | Get the program from a running bli application.
getProgram :: (BliSet t1,
               BliSet t2,
               BliSet t3,
               BliSet t4,
               Alias alias) => Bli t1 t2 t3 t4 alias (t1 Clause)
getProgram = (\(x,y,z,_,_,_) -> y) <$> get

-- | Get the schema from a running bli application.
getRelations  :: (BliSet t1,
                  BliSet t2,
                  BliSet t3,
                  BliSet t4,
                  Alias alias)
 => Bli t1 t2 t3 t4 alias (t2 RelDecl)
getRelations = (\(x,y,z,_,_,_) -> z) <$> get

getEntities :: (BliSet t1,
                BliSet t2,
                BliSet t3,
                BliSet t4,
                Alias alias)
 => Bli t1 t2 t3 t4 alias (t3 EntityDecl)
getEntities = (\(_,_,_,x,_,_) -> x) <$> get

getTypes :: (BliSet t1,
             BliSet t2,
             BliSet t3,
             BliSet t4,
             Alias alias)
 => Bli t1 t2 t3 t4 alias (t4 TypeDecl)
getTypes = (\(_,_,_,_,x,_) -> x) <$> get

getAliases :: (BliSet t1,
               BliSet t2,
               BliSet t3,
               BliSet t4,
               Alias alias)
 => Bli t1 t2 t3 t3 t4 alias (t4 TypeDecl)
getAliases = (\(_,_,_,_,_,x) -> x) <$> get

-- | Modify the options of a running bli application. 
modifyOpts :: (BliSet t1, 
               BliSet t2,
               BliSet t3,
               BliSet t4,
               Alias alias)
 => (AppConfig -> AppConfig) -> Bli t1 t2 t3 t4 alias ()
modifyOpts f = modify (\(x1,x2,x3,x4,x5,x6) -> (f x1, x2, x3, x4, x5, x6))

-- | Modify the program of a running bli application.
modifyProgram :: (BliSet t1, 
                  BliSet t2,
                  BliSet t3,
                  BliSet t4,
                  Alias alias)
 => (t1 Clause -> t1 Clause) -> Bli t1 t2 t3 t4 alias ()
modifyProgram f = modify (\(x1,x2,x3,x4,x5,x6) -> (x1, f x2, x3, x4, x4, x6))

-- | Modify the schema of a running bli application.
modifyRelations :: (BliSet t1, 
                    BliSet t2,
                    BliSet t3,
                    BliSet t4,
                    Alias alias)
 => (t2 SchemaEntry -> t2 SchemaEntry) -> Bli t1 t2 t3 t4 alias ()
modifyRelations f = modify (\(x1,x2,x3,x4,x5,x6) -> (x, y, f z))

modifyEntities :: (BliSet t1,
                   BliSet t2,
                   BliSet t3,
                   BliSet t4,
                   Alias alias)
 => (t3 RelDecl -> t3 RelDecl) -> Bli t1 t2 t3 t4 alias ()
modifyEntities f = modify (\(x1,x2,x3,x4,x5,x6) -> ())

modifyTypes :: (BliSet t1,
                BliSet t2,
                BliSet t3,
                BliSet t4,
                Alias alias)
 => (t4 TypeDecl -> t4 TypeDecl) ->
modifyTypes = modify (\(x1,x2,x3,x4,x5,x6) -> ())

modifyAliases :: (BliSet t1,
                  BliSet t2,
                  BliSet t3,
                  BliSet t4,
                  Alias alias)
 => (alias -> alias) -> Bli t1 t2 t3 t4 t5 alias ()
modifyAliases = modify (\(x1,x2,x4,x4,x5,x6) -> ())

-- | Set the program of a running bli application.
setProgram :: (BliSet t1, BliSet t2) => t1 Clause -> Bli t1 t2 ()
setProgram val = modify (\(x,y,z,_,_,_) -> (x, val, z))

-- | Set the options of a running bli application
setOpts :: (BliSet t1, BliSet t2) => AppConfig -> Bli t1 t2 ()
setOpts val = modify (\(x,y,z,_,_,_) -> (val, y, z))

-- | Set the schema of a running bli application
setSchema :: (BliSet t1, BliSet t2) => t2 SchemaEntry -> Bli t1 t2 ()
setSchema val = modify (\(x,y,z,_,_,_) -> (x, y, val))