
--
-- | Functons to compile bli prolog source files
--   into standalone executables, using cabal to compile
--   a haskell program that bundles the program definitions
--   and commands with the bli prolog runtime.
--

module Bli.Prolog.Compiler where

import Data.Bli.Prolog.Ast
import Distribution.Simple
import Distribution.Verbosity
import Distribution.PackageDescription.Parsec
import System.Cmd
import System.Directory

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Dynamically links
--   any imported modules in the program.
compileDyn :: BliProgram -> String -> IO ()
compileDyn = undefined

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Staticly links
--   any imported modules in the program.
compileStatic :: String -> String -> IO ()
compileStatic filePath outFile = do
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  -- cabal file to use for building our executable
  let buildFilePath = homeDir ++ "/code/bli-prolog/resc/"
  let cabalFileName = "build.cabal"
  let mainTemplate = "Main.hst"
  -- Todo: Need to handle relative v.s. absolute paths here.
  let sourceDest = currentDir ++ "/" ++ outFile
  
  -- Copy everything we need to tmp
  system $ "cp " ++ buildFilePath ++ cabalFileName ++ " /tmp"
  system $ "cp " ++ buildFilePath ++ mainTemplate ++ " /tmp/Main.hs"
  setCurrentDirectory "/tmp"
  --pkgDescr <- readGenericPackageDescription silent cabalFileName
  system $ "sed -i 's|<FILEPATH>|"++currentDir++"/"++filePath++"|g' Main.hs"
  system $ "cabal configure"
  system $ "cabal build"
  -- I haven't been able to get this to work yet
  -- defaultMainWithHooksNoReadArgs emptyUserHooks pkgDescr ["build"]
  system $ "cp " ++ "dist/build/build/build " ++ sourceDest
  return ()