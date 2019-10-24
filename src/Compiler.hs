
import System.Environment
import Bli.Prolog.Modules
import Bli.Prolog.Typechecking
import Bli.Prolog.Compiler
import Bli.Prolog.Compiler.Bytecode
import Bli.App.Config
import Control.Monad.Bli
import Bli.App.Config.Version
import Bli.App.Config.Executables
import System.IO
import System.Directory
import Data.List

fromJust (Just x) = x

compilerOpts = AppConfig {options = startOptions (fromJust $(getVersionFromCabal)), version = (fromJust $(getVersionFromCabal)) }

-- The main entrypoint for our compiler
main = do
  args       <- getArgs
  currentDir <- getCurrentDirectory
  case () of 
    _ | "--bytecode" `elem` args -> do
          let args' = filter (\x -> not $ isPrefixOf "--" x) args
          case length args' of 
            0 -> do
              printInvalidArgumentError
            1 -> do
              let arg1 = args' !! 0
              compileBytecode arg1 ""
            2 -> do
              let arg1 = args' !! 0
              let arg2 = args' !! 1
              compileBytecode arg1 arg2
      | "--dyn" `elem` args -> do
          let args' = filter (\x -> not $ isPrefixOf "--" x) args
          error "Dynamic compilation has not been implemented."
      | "--no-typecheck" `elem` args -> do
          let args' = filter (\x -> not $ isPrefixOf "--" x) args
          putStrLn "Warning: Typechecking has been disabled."
          case length args' of 
            0 -> do
              printInvalidArgumentError
            1 -> do
              let arg1 = args' !! 0
              compileStatic arg1 ""
            2 -> do
              let arg1 = args' !! 0
              let arg2 = args' !! 1
              compileStatic arg1 arg2
      | "--only-typecheck" `elem` args -> do
          let args' = filter (\x -> not $ isPrefixOf "--" x) args
          let arg = args' !! 0
          let filePath =
                case head arg of
                  '/' -> arg
                  '.' -> currentDir ++ "/" ++ tail arg
                  otherwise -> currentDir ++ "/" ++ arg
          maybeProgram <- getBliProgramFromFile filePath
          case maybeProgram of
            Nothing -> putStrLn "Error loading program."
            Just prog -> do
              result <- initBli compilerOpts $ typecheckBliProgram prog
              case result of
                Left errors -> do
                  putStrLn "Errors while typechecking program:"
                  mapM_ (\x -> putStrLn $ "  * "++show x) errors
                Right Ok    -> putStrLn "Program typechecks."
      | otherwise -> do
          case length args of
            0 -> do 
              printInvalidArgumentError
            1 -> do
              let arg1 = args !! 0
              case arg1 of
                "--help"  -> printHelpScreen
                "-h"      -> printHelpScreen
                otherwise -> do 
                  let filePath =
                        case head arg1 of
                          '/' -> arg1
                          '.' -> currentDir ++ "/" ++ tail arg1
                          otherwise -> currentDir ++ "/" ++ arg1
                  maybeProgram <- getBliProgramFromFile filePath
                  case maybeProgram of
                    Nothing -> putStrLn "Error loading program."
                    Just prog -> do
                      result <- initBli compilerOpts $ typecheckBliProgram prog
                      case result of 
                        Left errors -> do
                          putStrLn "Errors while typechecking program:"
                          mapM_ (\x -> putStrLn $ "  * "++show x) errors
                        Right Ok  -> do
                          putStrLn "Done typechecking."
                          compileStatic arg1 ""
            2 -> do
              let arg1 = args !! 0
              let arg2 = args !! 1
              let filePath =
                    case head arg1 of
                      '/' -> arg1
                      '.' -> currentDir ++ "/" ++ tail arg1
                      otherwise -> currentDir ++ "/" ++ arg1
              maybeProgram <- getBliProgramFromFile filePath
              case maybeProgram of
                Nothing -> putStrLn "Error loading program."
                Just prog -> do
                  result <- initBli compilerOpts $ typecheckBliProgram prog
                  case result of
                    Left errors -> do
                      putStrLn "Errors while typechecking program:"
                      mapM_ (\x -> putStrLn $ "  * "++show x) errors
                    Right Ok -> do
                      putStrLn "Done typechecking."
                      compileStatic arg1 arg2
  
printHelpScreen = putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
  ["bli-prolog compiler v"++version++", (C) Nathan Bedell 2019.",
   "   Visit https://github.com/Sintrastes/bli-prolog for more documentation on Bedelibry Prolog.",
   "",
   name++" [source_file]                   Compiles the specified source file to an executable",
   "                                     in the same directory, with the same name as the source",
   "                                     file, but without extension.",
   name++" [source_file] [output_file]     Compiles the specified source file to an exectuable",
   "                                     [output_file].",
   "",
   "Flags:",
   "  --help            Displays this help screen.",
   "  --dyn             Compiles the file, dynamically linknig any dependent modules.",
   "  --bytecode        Compiles to Bedelibry Prolog bytecode instead of linking into",
   "                    a static executable.",
   "  --no-typecheck    Skips the typechecking step.",
   "  --only-typecheck  Only typecheck the given file, skipping the compilation step."
  ]
 where Just version = $(getVersionFromCabal)
       name         = $(getCompilerNameFromCabal) 

printInvalidArgumentError = do
  let name = $(getCompilerNameFromCabal)
  putStrLn "Error parsing command line arguments. Must be called with one of the following formats:"
  putStrLn $ "  "++name++" [source_file] [output_file]"
  putStrLn $ "  "++name++" [source_file]"
  putStrLn "In case of the second format, the executable will be installed to the current directory,"
  putStrLn "with the same name as the source file, but without a .bpl extension."