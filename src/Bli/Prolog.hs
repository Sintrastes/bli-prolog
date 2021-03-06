
--
-- | The bli prolog runtime.
--

module Bli.Prolog where

import Data.Bli.Prolog.Schema
import Data.Bli.Prolog.Ast
import Bli.App
import Bli.Util
import Bli.App.Config
import Bli.App.Config.Data
import Bli.App.Config.Version
import Bli.App.Config.Features
import Bli.App.Cli
import Control.Monad.Bli
import Bli.Prolog.Typechecking (collectGoalVars)

-- | Helper function which splits a BliProgram into 
--   its commands, and it's declarations.
groupProgramCmdsDecls :: [BliCommand] -> ([BliCommand], [BliCommand])
groupProgramCmdsDecls xs = go xs ([],[])
  where go (cmd@(Assert goal):xs) (cmds, decls) = go xs (cmds, cmd:decls)
        go (cmd@(AssertClause clause):xs) (cmds, decls) = go xs (cmds, cmd:decls)
        go (cmd@(AssertSchema schema):xs) (cmds, decls) = go xs (cmds, cmd:decls)
        go (cmd@(Query goal):xs) (cmds, decls) = go xs (cmd:cmds, decls)
        go [] (cmds, decls) = (cmds, decls)

-- | Helper function to reorder a BliProgram into its logical order
--   for the purposes of sequentially typechecking.
reorderProg :: [BliCommand] -> [BliCommand]
reorderProg x = do
  let (types, relations, entities, clauses, goals) = groupSchemaClausesBpl x
  (map (\(t,b) -> AssertSchema $ Type b t) types) ++ (map (AssertSchema . uncurry TypeOf) entities)
        ++ (map (AssertSchema . (\(x,y) -> Pred NotStored x y []) ) relations) ++ (map AssertClause clauses)
        ++ (map Query $ map (\goal -> (collectGoalVars goal, goal)) goals)       

-- | Helper function to load all the declarations
--   in a prolog schema into the running Bli context. 
--   This should be defined in the Bli monad module.
loadDecs :: [BliCommand] -> Bli Bool
loadDecs prog = do
    let (types,relations,entities,clauses) = groupSchemaClauses prog
    r1 <- newFacts     clauses
    r2 <- newTypes     types
    r3 <- newEntities   entities
    r4 <- newRelations relations
    return $ r1 && r2 && r3 && r4 

fromJust (Just x) = x

-- Configuration to use the the bli prolog runtime.
runtimeCfg = AppConfig {
    options = startOptions (fromJust $(getVersionFromCabal))
  , version = (fromJust $(getVersionFromCabal))
  , languageOptions = defaultLanguageOptions
}

-- | Takes a BliProgram which has already been type-checked, 
--   and executes all of the commands in the program.
executeProgram :: [BliCommand] -> IO ()
executeProgram prog = initBli runtimeCfg $ do
  let progCommands = prog
  let (commands, declarations) = groupProgramCmdsDecls $ (reverse progCommands)
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