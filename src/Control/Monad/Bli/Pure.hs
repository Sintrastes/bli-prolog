
module Control.Monad.Bli.Pure where

--
-- | A pure version of the Bli monad. 
--   Can be used to favilitate the use of bli prolog
--   as an EDSL witihn Haskell
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.

import Data.Prolog.Ast
import Control.Applicative
import Data.Schema
import Control.Monad.State.Lazy
import Bli.App.Config (Options)
import qualified Control.Monad.Bli as Bli

-- | A monad for wrapping pure computations done (and run) in bli prolog.
type Bli a = State (Options, Program, Schema) a

-- | Run a pure Bli computation with some initial state.
runBli :: Options -> Program -> Schema -> Bli a -> a
runBli options program schema app = evalState app (options, program, schema)

-- | Get the options from a pure bli computation.
getOpts    :: Bli Options
getOpts = (\(x,y,z) -> x) <$> get

-- | Get the program from a pure bli computation.
getProgram :: Bli Program
getProgram = (\(x,y,z) -> y) <$> get

-- | Get the schema from a pure bli computation.
getSchema  :: Bli Schema
getSchema = (\(x,y,z) -> z) <$> get

-- | Modify the options of a pure bli computation. 
modifyOpts :: (Options -> Options) -> Bli ()
modifyOpts f = modify (\(x,y,z) -> (f x, y, z))

-- | Modify the program of a pure bli computation.
modifyProgram :: (Program -> Program) -> Bli ()
modifyProgram f = modify (\(x,y,z) -> (x, f y, z))

-- | Modify the schema of a pure bli computation.
modifySchema :: (Schema -> Schema) -> Bli ()
modifySchema f = modify (\(x,y,z) -> (x, y, f z))

-- | Set the program of a pure bli computation.
setProgram :: Program -> Bli ()
setProgram val = modify (\(x,y,z) -> (x, val, z))

-- | Set the options of a pure bli computation.
setOpts :: Options -> Bli ()
setOpts val = modify (\(x,y,z) -> (val, y, z))

-- | Set the schema of a pure bli computation.
setSchema :: Schema  -> Bli ()
setSchema val = modify (\(x,y,z) -> (x, y, val))

-- Helper function to go from the pure to the impure version of the Bli monad.
liftFromPure :: Bli a -> Bli.Bli a
liftFromPure x = do
  o <- Bli.getOpts
  c <- Bli.getProgram
  s <- Bli.getSchema
  let (opts, clauses, schema) = execState x (o,c,s)
  Bli.setOpts opts
  Bli.setProgram clauses
  Bli.setSchema schema
  return $ runBli opts clauses schema x