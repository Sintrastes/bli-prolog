
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
import Control.Monad.State.Lazy
import Data.Schema
import Bli.App.Config (Options)
import qualified Control.Monad.Bli.Pure.Generic as Generic
import qualified Control.Monad.Bli as Bli

-- | A monad for wrapping pure computations done (and run) in bli prolog.
type Bli a = Generic.Bli [] a

-- | Run a pure Bli computation with some initial state.
runBli :: Options -> Program -> Schema -> Bli a -> a
runBli = Generic.runBli

-- | Get the options from a pure bli computation.
getOpts    :: Bli Options
getOpts = Generic.getOpts

-- | Get the program from a pure bli computation.
getProgram :: Bli Program
getProgram = Generic.getProgram

-- | Get the schema from a pure bli computation.
getSchema  :: Bli Schema
getSchema = Generic.getSchema

-- | Modify the options of a pure bli computation. 
modifyOpts :: (Options -> Options) -> Bli ()
modifyOpts = Generic.modifyOpts

-- | Modify the program of a pure bli computation.
modifyProgram :: (Program -> Program) -> Bli ()
modifyProgram = Generic.modifyProgram

-- | Modify the schema of a pure bli computation.
modifySchema :: (Schema -> Schema) -> Bli ()
modifySchema = Generic.modifySchema

-- | Set the program of a pure bli computation.
setProgram :: Program -> Bli ()
setProgram = Generic.setProgram

-- | Set the options of a pure bli computation.
setOpts :: Options -> Bli ()
setOpts = Generic.setOpts

-- | Set the schema of a pure bli computation.
setSchema :: Schema  -> Bli ()
setSchema = Generic.setSchema

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