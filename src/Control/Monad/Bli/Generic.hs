
module Control.Monad.Bli.Generic where

-- | Generic version of the Bli monad

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Prolog.Ast
import Data.Schema
import Bli.App.Config

type Bli t a = StateT (Options, t Clause, t SchemaEntry) IO a

-- | Lift io computations into the Bli monad.
io :: IO a -> Bli t a
io = lift

-- | Run a Bli application with some initial state.
runBli :: Options -> t Clause -> t SchemaEntry -> Bli t a -> IO a
runBli options program schema app = evalStateT app (options, program, schema)

-- | Get the options from a running bli application
getOpts    :: Bli t Options
getOpts = (\(x,y,z) -> x) <$> get

-- | Get the program from a running bli application.
getProgram :: Bli t (t Clause)
getProgram = (\(x,y,z) -> y) <$> get

-- | Get the schema from a running bli application.
getSchema  :: Bli t (t SchemaEntry)
getSchema = (\(x,y,z) -> z) <$> get

-- | Modify the options of a running bli application. 
modifyOpts :: (Options -> Options) -> Bli t ()
modifyOpts f = modify (\(x,y,z) -> (f x, y, z))

-- | Modify the program of a running bli application.
modifyProgram :: (t Clause -> t Clause) -> Bli t ()
modifyProgram f = modify (\(x,y,z) -> (x, f y, z))

-- | Modify the schema of a running bli application.
modifySchema :: (t SchemaEntry -> t SchemaEntry) -> Bli t ()
modifySchema f = modify (\(x,y,z) -> (x, y, f z))

-- | Set the program of a running bli application.
setProgram :: t Clause -> Bli t ()
setProgram val = modify (\(x,y,z) -> (x, val, z))

-- | Set the options of a running bli application
setOpts :: Options -> Bli t ()
setOpts val = modify (\(x,y,z) -> (val, y, z))

-- | Set the schema of a running bli application
setSchema :: t SchemaEntry  -> Bli t ()
setSchema val = modify (\(x,y,z) -> (x, y, val))