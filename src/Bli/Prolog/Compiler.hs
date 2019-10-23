{-# LANGUAGE NoOverloadedStrings #-}

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
import Control.Monad
import System.Cmd
import System.Process
import System.Directory
import System.Exit
import Data.List.Split

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Dynamically links
--   any imported modules in the program.
compileDyn :: BliProgram -> String -> IO ()
compileDyn = undefined

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Staticly links
--   any imported modules in the program.
compileStatic :: String -> String -> IO ()
compileStatic filePath outFilePath = do
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  -- cabal file to use for building our executable
  let buildFilePath = homeDir ++ "/code/bli-prolog/resc/"
  let cabalFileName = "build.cabal"
  let mainTemplate = "Main.hst"
  let inputFileWithoutExtension = 
        head $ splitOn "." $ last $ splitOn "/" filePath
  let sourceDest = 
        case outFilePath of
          "" -> currentDir ++ "/" ++ inputFileWithoutExtension
          otherwise ->
            case head outFilePath of
              -- This is an absolute path
              '/' -> outFilePath 
              -- This is a relative path
              '.' -> currentDir ++ (tail outFilePath)
              -- If the path does not start with a '/', assume this is a relative path.
              otherwise -> currentDir ++ "/" ++ outFilePath
  let outFile = last $ splitOn "/" sourceDest
  
  -- Copy everything we need to tmp
  putStrLn "Compiling..."
  copyingResult <- join $ waitForProcess <$> runCommand ( 
                     "cp " ++ buildFilePath ++ cabalFileName ++ " /tmp && \\\n" ++
                     "cp " ++ buildFilePath ++ mainTemplate ++ " /tmp/Main.hs")
  case copyingResult of
    ExitFailure code -> do
      putStrLn $ "There was an error copying files to /tmp. Exit code " ++ show code
    ExitSuccess -> do
      cabalResult <- join $ waitForProcess <$> runCommand (
         "cd /tmp && \\\n"++
         "sed -i 's|<FILEPATH>|"++currentDir++"/"++filePath++"|g' Main.hs && \\\n"++
         "cabal configure > /dev/null 2>&1 && \\\n"++
         "cabal build > /dev/null 2>&1 ")
  -- I haven't been able to get this to work yet
  -- defaultMainWithHooksNoReadArgs emptyUserHooks pkgDescr ["build"]
  -- Make a smaller executable.
      case cabalResult of
        ExitFailure code -> putStrLn $ "There was an error compiling. Exit code: " ++ show code
        ExitSuccess -> do 
           putStrLn "Done compiling."
           join $ waitForProcess <$> runCommand (
              "strip /tmp/dist/build/build/build && \n" ++
              "cp " ++ "/tmp/dist/build/build/build " ++ sourceDest)
           -- Cleanup
           putStrLn "Cleaning up build files..."
           join $ waitForProcess <$> runCommand ( 
             "rm -rf /tmp/dist && \n" ++
             "rm /tmp/Main.hs && \n" ++
             "rm -rf /tmp/build.cabal && \n" ++
             "echo \"Successfully built executable " ++ outFile ++ ".\"")
           return ()
