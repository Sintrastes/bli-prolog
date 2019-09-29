
module Control.Monad.Bli where

--
-- | A monad for IO computations preformed in the context of a 
--   running bli prolog session.
--
--   Note: If used within the same application as Control.Monad.Bli,
--   must be imported qualified to avoid name conflicts.
--

import Data.Prolog.Ast
import Data.Schema
import Bli.App.Config (Options)
import qualified Control.Monad.Bli.Generic as Generic

-- | A monad for wrapping computations done (and run) in bli prolog.
type Bli a = Generic.Bli [] a

-- | Lift io computations into the Bli monad.
io :: IO a -> Bli a
io = Generic.io

-- | Run a Bli application with some initial state.
runBli :: Options -> Program -> Schema -> Bli a -> IO a
runBli = Generic.runBli

-- | Get the options from a running bli application
getOpts    :: Bli Options
getOpts = Generic.getOpts

-- | Get the program from a running bli application.
getProgram :: Bli Program
getProgram = Generic.getProgram

-- | Get the schema from a running bli application.
getSchema  :: Bli Schema
getSchema = Generic.getSchema

-- | Modify the options of a running bli application. 
modifyOpts :: (Options -> Options) -> Bli ()
modifyOpts = Generic.modifyOpts

-- | Modify the program of a running bli application.
modifyProgram :: (Program -> Program) -> Bli ()
modifyProgram = Generic.modifyProgram

-- | Modify the schema of a running bli application.
modifySchema :: (Schema -> Schema) -> Bli ()
modifySchema = Generic.modifySchema

-- | Set the program of a running bli application.
setProgram :: Program -> Bli ()
setProgram = Generic.setProgram

-- | Set the options of a running bli application
setOpts :: Options -> Bli ()
setOpts = Generic.setOpts

-- | Set the schema of a running bli application
setSchema :: Schema  -> Bli ()
setSchema = Generic.setSchema