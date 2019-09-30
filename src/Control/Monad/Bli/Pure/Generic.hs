
module Control.Monad.Bli.Pure.Generic where

-- | Generic version of the pure Bli monad

import Data.Prolog.Ast
import Control.Applicative
import Data.Schema
import Data.BliSet
import Control.Monad.State.Lazy
import Bli.App.Config (Options)
import qualified Control.Monad.Bli.Generic as Bli

-- | A monad for wrapping pure computations done (and run) in bli prolog.
type Bli t1 t2 a = State (Options, t1 Clause, t2 SchemaEntry) a

-- | Run a pure Bli computation with some initial state.
runBli :: (BliSet t1, BliSet t2) => Options -> t1 Clause -> t2 SchemaEntry -> Bli t1 t2 a -> a
runBli options program schema app = evalState app (options, program, schema)

-- | Get the options from a pure bli computation.
getOpts    :: (BliSet t1, BliSet t2) => Bli t1 t2 Options
getOpts = (\(x,y,z) -> x) <$> get

-- | Get the program from a pure bli computation.
getProgram :: (BliSet t1, BliSet t2) => Bli t1 t2 (t1 Clause)
getProgram = (\(x,y,z) -> y) <$> get

-- | Get the schema from a pure bli computation.
getSchema  :: (BliSet t1, BliSet t2) => Bli t1 t2 (t2 SchemaEntry)
getSchema = (\(x,y,z) -> z) <$> get

-- | Modify the options of a pure bli computation. 
modifyOpts :: (BliSet t1, BliSet t2) => (Options -> Options) -> Bli t1 t2 ()
modifyOpts f = modify (\(x,y,z) -> (f x, y, z))

-- | Modify the program of a pure bli computation.
modifyProgram :: (BliSet t1, BliSet t2) => (t1 Clause -> t1 Clause) -> Bli t1 t2 ()
modifyProgram f = modify (\(x,y,z) -> (x, f y, z))

-- | Modify the schema of a pure bli computation.
modifySchema :: (BliSet t1, BliSet t2) => (t2 SchemaEntry -> t2 SchemaEntry) -> Bli t1 t2 ()
modifySchema f = modify (\(x,y,z) -> (x, y, f z))

-- | Set the program of a pure bli computation.
setProgram :: (BliSet t1, BliSet t2) => t1 Clause -> Bli t1 t2 ()
setProgram val = modify (\(x,y,z) -> (x, val, z))

-- | Set the options of a pure bli computation.
setOpts :: (BliSet t1, BliSet t2) => Options -> Bli t1 t2 ()
setOpts val = modify (\(x,y,z) -> (val, y, z))

-- | Set the schema of a pure bli computation.
setSchema :: (BliSet t1, BliSet t2) => t2 SchemaEntry -> Bli t1 t2 ()
setSchema val = modify (\(x,y,z) -> (x, y, val))

-- Helper function to go from the pure to the impure version of the Bli monad.
liftFromPure :: (BliSet t1, BliSet t2) => Bli t1 t2 a -> Bli.Bli t1 t2 a
liftFromPure x = do
  o <- Bli.getOpts
  c <- Bli.getProgram
  s <- Bli.getSchema
  let (opts, clauses, schema) = execState x (o,c,s)
  Bli.setOpts opts
  Bli.setProgram clauses
  Bli.setSchema schema
  return $ runBli o c s x
  