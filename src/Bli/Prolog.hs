
--
-- | The bli prolog runtime.
--

module Bli.Prolog where

import Data.Bli.Prolog.Ast
import Bli.App
import Bli.Util
import Bli.App.Config
import Bli.App.Config.Version
import Bli.App.Cli
import Control.Monad.Bli

-- | Helper function which splits a BliProgram into 
--   its commands, and it's declarations.
groupProgramCmdsDecls :: BliProgram -> (BliProgram, BliProgram)
groupProgramCmdsDecls xs = go xs ([],[])
  where go (cmd@(AssertMode goal):xs) (cmds, decls) = go xs (cmds, cmd:decls)
        go (cmd@(AssertClause clause):xs) (cmds, decls) = go xs (cmds, cmd:decls)
        go (cmd@(AssertSchema schema):xs) (cmds, decls) = go xs (cmds, cmd:decls)
        go (cmd@(QueryMode goal):xs) (cmds, decls) = go xs (cmd:cmds, decls)
        go (cmd@(LambdaQuery goal):xs) (cmds, decls) = go xs (cmd:cmds, decls)
        go [] (cmds, decls) = (cmds, decls)

-- | Helper function to load all the declarations
--   in a prolog schema into the running Bli context. 
--   This should be defined in the Bli monad module.
loadDecs :: BliProgram -> Bli Bool
loadDecs prog = do
    let (types,relations,entities,clauses) = groupSchemaClauses prog
    r1 <- newFacts     clauses
    r2 <- newTypes     types
    r3 <- newEntities   entities
    r4 <- newRelations relations
    return $ r1 && r2 && r3 && r4 

fromJust (Just x) = x

-- Configuration to use the the bli prolog runtime.
runtimeCfg = AppConfig {options = startOptions (fromJust $(getVersionFromCabal)), version = (fromJust $(getVersionFromCabal)) }

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
executeCommand = 
 -- For now this is the same as the function we use at the REPL, but
 -- this might change in the future.
    processBliCommandRepl 