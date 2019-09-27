
module Control.Monad.Bli.Pure where

--
-- | A pure version of the Bli monad. 
--   Can be used to favilitate the use of bli prolog
--   as an EDSL witihn Haskell
--

import Data.Prolog.Ast
import Control.Applicative
import Data.Schema

-- | A monad for wrapping computations done (and run) in bli prolog.
---  Note: We should also have a pure version of this later.
type Bli a = StateT (Options, Program, Schema) IO a

-- | Lift io computations into the Bli monad.
io :: IO a -> Bli a
io = lift

-- | Run a Bli application with some initial state.
runBli :: Options -> Program -> Schema -> Bli a -> IO a
runBli options program schema app = evalStateT app (options, program, schema)

-- | Get the options from a running bli application
getOpts    :: Bli Options
getOpts = fst <$> get

-- | Get the program from a running bli application.
getProgram :: Bli Program
getProgram = snd <$> get

-- | Get the schema from a running bli application.
getSchema  :: Bli Schema
getSchema = (!! 3) <$> get

-- | Modify the options of a running bli application. 
modifyOpts :: (Options -> Options) -> Bli ()
modifyOpts f = modify (\(x,y,z) -> (f x, y, z))

-- | Modify the program of a running bli application.
modifyProgram :: (Program -> Program) -> Bli ()
modifyProgram f = modify (\(x,y,z) -> (x, f y, z))

-- | Modify the schema of a running bli application.
modifySchema :: (Schema -> Schema) -> Bli ()
modifySchema f = modify (\(x,y,z) -> (x, y, f z))

-- | Set the program of a running bli application.
setProgram :: Program -> Bli ()
setProgram val = modify (\(x,y,z) -> (val, y, z))

-- | Set the options of a running bli application
setOpts :: Options -> Bli ()
setOpts val = modify (\(x,y,z) -> (x, val, z))

-- | Set the schema of a running bli application
setSchema :: Schema  -> Bli ()
setSchema val = modify (\(x,y,z) -> (x, y, val))