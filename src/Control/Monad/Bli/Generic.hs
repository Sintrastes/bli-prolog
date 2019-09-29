
module Control.Monad.Bli.Generic where

-- | Generic version of the Bli monad

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Prolog.Ast
import Data.Schema
import Bli.App.Config
import Data.BliSet

type Bli t1 t2 a = StateT (Options, t1 Clause, t2 SchemaEntry) IO a

-- | Lift io computations into the Bli monad.
io :: (BliSet t1, BliSet t2) => IO a -> Bli t1 t2 a
io = lift

-- | Run a Bli application with some initial state.
runBli :: (BliSet t1, BliSet t2) => Options -> t1 Clause -> t2 SchemaEntry -> Bli t1 t2 a -> IO a
runBli options program schema app = evalStateT app (options, program, schema)

-- | Get the options from a running bli application
getOpts    :: (BliSet t1, BliSet t2) => Bli t1 t2 Options
getOpts = (\(x,y,z) -> x) <$> get

-- | Get the program from a running bli application.
getProgram :: (BliSet t1, BliSet t2) => Bli t1 t2 (t1 Clause)
getProgram = (\(x,y,z) -> y) <$> get

-- | Get the schema from a running bli application.
getSchema  :: (BliSet t1, BliSet t2) => Bli t1 t2 (t2 SchemaEntry)
getSchema = (\(x,y,z) -> z) <$> get

-- | Modify the options of a running bli application. 
modifyOpts :: (BliSet t1, BliSet t2) => (Options -> Options) -> Bli t1 t2 ()
modifyOpts f = modify (\(x,y,z) -> (f x, y, z))

-- | Modify the program of a running bli application.
modifyProgram :: (BliSet t1, BliSet t2) => (t1 Clause -> t1 Clause) -> Bli t1 t2 ()
modifyProgram f = modify (\(x,y,z) -> (x, f y, z))

-- | Modify the schema of a running bli application.
modifySchema :: (BliSet t1, BliSet t2) => (t2 SchemaEntry -> t2 SchemaEntry) -> Bli t1 t2 ()
modifySchema f = modify (\(x,y,z) -> (x, y, f z))

-- | Set the program of a running bli application.
setProgram :: (BliSet t1, BliSet t2) => t1 Clause -> Bli t1 t2 ()
setProgram val = modify (\(x,y,z) -> (x, val, z))

-- | Set the options of a running bli application
setOpts :: (BliSet t1, BliSet t2) => Options -> Bli t1 t2 ()
setOpts val = modify (\(x,y,z) -> (val, y, z))

-- | Set the schema of a running bli application
setSchema :: (BliSet t1, BliSet t2) => t2 SchemaEntry -> Bli t1 t2 ()
setSchema val = modify (\(x,y,z) -> (x, y, val))