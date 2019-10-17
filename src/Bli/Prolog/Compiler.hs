
--
-- | Functons to compile bli prolog source files
--   into standalone executables, using cabal to compile
--   a haskell program that bundles the program definitions
--   and commands with the bli prolog runtime.
--

module Bli.Prolog.Compiler where

import Data.Bli.Prolog.Ast
import Distribution.Simple
import Distribution.PackageDescription.Parsec

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Dynamically links
--   any imported modules in the program.
compileDyn :: BliProgram -> String -> IO ()
compileDyn = undefined

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Staticly links
--   any imported modules in the program.
compileStatic :: BliProgram -> String -> IO ()
compileStatic = undefined