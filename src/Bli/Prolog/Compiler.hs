{-# LANGUAGE NoOverloadedStrings #-}

--
-- | Functons to compile bli prolog source files
--   into standalone executables, using cabal to compile
--   a haskell program that bundles the program definitions
--   and commands with the bli prolog runtime.
--

module Bli.Prolog.Compiler where

import Prelude hiding (writeFile)
import Bli.Prolog.Compiler.Bytecode
import Data.Bli.Prolog.Ast
import Distribution.Simple
import Distribution.Verbosity
import Distribution.PackageDescription.Parsec
import Control.Monad
import Data.ByteString (writeFile)
import System.Process
import System.Directory
import System.Exit
import Data.List.Split
import Bli.Prolog.Modules
import Control.Monad.Bli
import Control.Monad.IO.Class
import Bli.App.Config
import Bli.App.Config.Features
import Bli.App.Config.Data
import Bli.Prolog.Compiler.TH
import Bli.App.Config.Version

fromJust (Just x) = x

compilerOpts = AppConfig {
    options = startOptions (fromJust $(getVersionFromCabal))
  , version = (fromJust $(getVersionFromCabal))
  , languageOptions = defaultLanguageOptions
}

compileBytecode :: String -> String -> Bli ()
compileBytecode filePath outFilePath = do
  liftIO $ putStrLn "Compiling file to bytecode..."
  currentDir <- liftIO $ getCurrentDirectory
  maybeProgram <- getBliProgramFromFile filePath
  let inputFileWithoutExtension = 
        head $ splitOn "." $ last $ splitOn "/" filePath
  let sourceDest = 
        case outFilePath of
          "" -> currentDir ++ "/" ++ inputFileWithoutExtension ++ ".bc"
          otherwise ->
            case head outFilePath of
              -- This is an absolute path
              '/' -> outFilePath 
              -- This is a relative path
              '.' -> currentDir ++ (tail outFilePath)
              -- If the path does not start with a '/', assume this is a relative path.
              otherwise -> currentDir ++ "/" ++ outFilePath
  case maybeProgram of
    Just program -> do
      let bytecode = toBytecode program
      liftIO $ writeFile sourceDest bytecode
      liftIO $ putStrLn "Done compiling."
    Nothing -> do
      error "Error compiling file to bytecode."

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Dynamically links
--   any imported modules in the program.
compileDyn :: BliProgram -> String -> Bli ()
compileDyn = undefined

-- | Compiles the given BliProgram into an executable 
--   at the specified filepath. Staticly links
--   any imported modules in the program.
compileStatic :: String -> String -> Bli ()
compileStatic filePath outFilePath = do
  homeDir <- liftIO $ getHomeDirectory
  currentDir <- liftIO $ getCurrentDirectory
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
  liftIO $ putStrLn "Compiling..."
  copyingResult <- liftIO $ join $ waitForProcess <$> runCommand ( 
                     "cp " ++ buildFilePath ++ cabalFileName ++ " /tmp && \\\n" ++
                     "cp " ++ buildFilePath ++ mainTemplate ++ " /tmp/Main.hs")
  case copyingResult of
    ExitFailure code -> do
      liftIO $ putStrLn $ "There was an error copying files to /tmp. Exit code " ++ show code
    ExitSuccess -> do
      cabalResult <- liftIO $ join $ waitForProcess <$> runCommand (
         "cd /tmp && \\\n"++
         "sed -i 's|<FILEPATH>|"++currentDir++"/"++filePath++"|g' Main.hs && \\\n"++
         "cabal configure --enable-shared -O2 > /dev/null 2>&1 && \\\n"++
         "cabal build > /dev/null 2>&1 ")
  -- I haven't been able to get this to work yet
  -- defaultMainWithHooksNoReadArgs emptyUserHooks pkgDescr ["build"]
  -- Make a smaller executable.
      case cabalResult of
        ExitFailure code -> liftIO $ putStrLn $ "There was an error compiling. Exit code: " ++ show code
        ExitSuccess -> do 
           liftIO $ putStrLn "Done compiling."
           liftIO $ join $ waitForProcess <$> runCommand (
              "strip /tmp/dist/build/build/build && \n" ++
              "cp " ++ "/tmp/dist/build/build/build " ++ sourceDest)
           -- Cleanup
           liftIO $ putStrLn "Cleaning up build files..."
           liftIO $ join $ waitForProcess <$> runCommand ( 
             "rm -rf /tmp/dist && \n" ++
             "rm /tmp/Main.hs && \n" ++
             "rm -rf /tmp/build.cabal && \n" ++
             "echo \"Successfully built executable " ++ outFile ++ ".\"")
           return ()
