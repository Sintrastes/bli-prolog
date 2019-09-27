
module Control.Monad.Bli where

--
-- | A monad for IO computations preformed in the context of a 
--   running bli prolog session.
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.
--

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Prolog.Ast
import Data.Schema
import Control.Applicative
import Bli.App.Config (Options)

-- | A monad for wrapping computations done (and run) in bli prolog.
type Bli a = StateT (Options, Program, Schema) IO a

-- | Lift io computations into the Bli monad.
io :: IO a -> Bli a
io = lift

-- | Run a Bli application with some initial state.
runBli :: Options -> Program -> Schema -> Bli a -> IO a
runBli options program schema app = evalStateT app (options, program, schema)

-- | Get the options from a running bli application
getOpts    :: Bli Options
getOpts = (\(x,y,z) -> x) <$> get

-- | Get the program from a running bli application.
getProgram :: Bli Program
getProgram = (\(x,y,z) -> y) <$> get

-- | Get the schema from a running bli application.
getSchema  :: Bli Schema
getSchema = (\(x,y,z) -> z) <$> get

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
setProgram val = modify (\(x,y,z) -> (x, val, z))

-- | Set the options of a running bli application
setOpts :: Options -> Bli ()
setOpts val = modify (\(x,y,z) -> (val, y, z))

-- | Set the schema of a running bli application
setSchema :: Schema  -> Bli ()
setSchema val = modify (\(x,y,z) -> (x, y, val))