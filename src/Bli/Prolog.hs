
--
-- | The bli prolog runtime.
--

module Bli.Prolog where

import Data.Bli.Prolog.Ast
import Bli.App
import Control.Monad.Bli

-- | Helper function which splits a BliProgram into 
--   its commands, and it's declarations.
groupProgramCmdsDecls :: BliProgram -> (BliProgram, BliProgram)
groupProgramCmdsDecls = undefined

-- | Helper function to load all the declarations
--   in a prolog schema into the running Bli context. 
--   This should be defined in the Bli monad module.
loadDecs = undefined

-- Configuration to use the the bli prolog runtime.
runtimeCfg = undefined

-- | Takes a BliProgram which has already been type-checked, 
--   and executes all of the commands in the program.
executeProgram :: BliProgram -> IO ()
executeProgram prog = initBli runtimeCfg $ do
  let (commands, declarations) = groupProgramCmdsDecls prog
  result <- loadDecs declarations
  case result of 
    False -> error "Failed loading declarations." 
    True -> do
      mapM_ executeCommand commands

-- | Executes a single bli command in the given bedelibry context.
executeCommand :: BliCommand -> Bli ()
executeCommand = undefined