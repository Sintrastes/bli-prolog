
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
type Bli t a = State (Options, t Clause, t SchemaEntry) a

-- | Run a pure Bli computation with some initial state.
runBli :: Options -> t Clause -> t SchemaEntry -> Bli t a -> a
runBli options program schema app = evalState app (options, program, schema)

-- | Get the options from a pure bli computation.
getOpts    :: BliSet t => Bli t Options
getOpts = (\(x,y,z) -> x) <$> get

-- | Get the program from a pure bli computation.
getProgram :: BliSet t => Bli t (t Clause)
getProgram = (\(x,y,z) -> y) <$> get

-- | Get the schema from a pure bli computation.
getSchema  :: BliSet t => Bli t (t SchemaEntry)
getSchema = (\(x,y,z) -> z) <$> get

-- | Modify the options of a pure bli computation. 
modifyOpts :: BliSet t => (Options -> Options) -> Bli t ()
modifyOpts f = modify (\(x,y,z) -> (f x, y, z))

-- | Modify the program of a pure bli computation.
modifyProgram :: BliSet t => (t Clause -> t Clause) -> Bli t ()
modifyProgram f = modify (\(x,y,z) -> (x, f y, z))

-- | Modify the schema of a pure bli computation.
modifySchema :: BliSet t => (t SchemaEntry -> t SchemaEntry) -> Bli t ()
modifySchema f = modify (\(x,y,z) -> (x, y, f z))

-- | Set the program of a pure bli computation.
setProgram :: BliSet t => t Clause -> Bli t ()
setProgram val = modify (\(x,y,z) -> (x, val, z))

-- | Set the options of a pure bli computation.
setOpts :: BliSet t => Options -> Bli t ()
setOpts val = modify (\(x,y,z) -> (val, y, z))

-- | Set the schema of a pure bli computation.
setSchema :: BliSet t => t SchemaEntry -> Bli t ()
setSchema val = modify (\(x,y,z) -> (x, y, val))

-- Helper function to go from the pure to the impure version of the Bli monad.
liftFromPure :: BliSet t => Bli t a -> Bli.Bli t a
liftFromPure x = do
  o <- Bli.getOpts
  c <- Bli.getProgram
  s <- Bli.getSchema
  let (opts, clauses, schema) = execState x (o,c,s)
  Bli.setOpts opts
  Bli.setProgram clauses
  Bli.setSchema schema
  return $ runBli opts clauses schema x