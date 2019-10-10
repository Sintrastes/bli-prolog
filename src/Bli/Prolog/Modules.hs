
module Bli.Prolog.Modules where

--
-- | Implementation of the module system for bli prolog.
--

import Data.Bli.Prolog.Ast
import Control.Monad.Bli

-- | Helper function to load a bli program
--   into the given context.
--
--   Note: I might want to put this somewhere else later.
loadBliProgram :: BliProgram -> Bli ()
loadBliProgram prog = undefined

-- | Helper function to load a Bli program from a file.
--   Returns a boolean flag to indicate success/failure.
-- 
--   Note: Again, I may want to put this somewhere else later.
loadBliProgramFromFile :: String -> Bli Bool
loadBliProgramFromFile filePath = undefined

-- | loads all of the modules imported in a bli prolog file.
loadModules :: BliProgram -> Maybe BliProgram
loadModules prog = undefined

-- | Loads the modules imported in a blit prolog file recursively,
--   checking for cyclical imports. 
loadModulesRec :: BliProgram -> Maybe BliProgram
loadModulesRec prog = undefined

