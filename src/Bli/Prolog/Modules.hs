
module Bli.Prolog.Modules where

--
-- | Implementation of the module system for bli prolog.
--

import Data.Bli.Prolog.Ast
import Control.Monad.Bli
import Bli.App.Config (getBliModuleData)
import Data.Yaml
import Bli.Util (groupClauses)
import Data.List.Ordered

-- | Helper function to load a bli program
--   into the given context.
--
--   Note: I might want to put this somewhere else later.
loadBliProgram :: BliProgram -> Bli ()
loadBliProgram prog = do
  maybeModuleData <- liftIO $ getBliModuleData
  case maybeModuleData of
    Nothing -> error "Error parsing module file."
    Just modData -> go prog [] modData
 where go prog loadedMods modData = do
          let (types, 
               relations,
               entities,
               clauses,
               moduleNames) 
            = groupClauses prog
          -- TODO: Do type-checking here.
          
          -- Add everything to the local context.
          newTypes types
          newRelations relations
          newEntities entities
          newClauses clauses
                    
          case moduleNames `subset` loadedMods of
            True  -> return ()
            False -> do
              forM (\modName -> do
                      let maybeModPath = lookup modName modData
                      case maybeModPath of
                        Nothing -> error "Module has not been declared."
                        Just modPath -> do
                          maybeLoadedMod <- loadBliProgramFromFile modPath
                          case maybeLoadedMod of
                            Nothing -> error "Error loading module."
                            Just loadedMod -> do
                              loadBliProgram loadedMod
                   ) moduleNames

-- | Helper function to load a Bli program from a file.
--   Returns a boolean flag to indicate success/failure.
-- 
--   Note: Again, I may want to put this somewhere else later.
loadBliProgramFromFile :: String -> Bli (Maybe BliProgram)
loadBliProgramFromFile filePath = undefined

-- | loads all of the modules imported in a bli prolog file.
loadModules :: BliProgram -> Maybe BliProgram
loadModules prog = undefined

-- | Loads the modules imported in a blit prolog file recursively,
--   checking for cyclical imports. 
loadModulesRec :: BliProgram -> Maybe BliProgram
loadModulesRec prog = undefined

