
--
-- | Logic for the command line interface of bli-prolog
-- 

module Bli.App.Cli where

import Control.Monad.Bli
import qualified Control.Monad.Bli.Pure as Pure
import Bli.App
import Bli.App.Api
import Bli.App.Json
import Data.List.Split
import Bli.App.Colors
import Bli.App.Config
import Prolog.Analysis
import Data.Prolog.Ast
import Data.Schema
import Data.Aeson
import Prolog.Parser
import Prolog.Interp
import Data.List
import System.Console.CmdArgs as CA hiding (program)
import System.Console.Readline

-- Helper function to get the file extension of a filepath.
fileExtension :: String -> String
fileExtension = undefined

-- | Helper function to process bli-prolog commands in a running application.
processCliInput :: String -> Bli ()
processCliInput input' = do
          -- Get schema, clauses, and options from context.
          schema <- getSchema
          clauses <- getProgram
          opts <- getOpts
          let colorOpts = not $ nocolor opts
          -- Prepend the user input with the required symbol for
          -- our parsers
          let input = "?- " ++ input'
          -- Parse and handle the command
          let parserOutput = parseBliCommand input
          case parserOutput of
            Left err -> do io $ putStrLn ((red colorOpts "Error")++" parsing query string:")
                           io $ putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
                           io $ putStrLn $ 
                             (yellow colorOpts
                                "All bli prolog commands end with either a '.' or an '!'.")
            Right command -> do
              result <- Pure.liftFromPure $ processBliCommand command
              case result of
                Result_QueryFail (AtomsNotInSchema atoms) -> do
                       io $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                       io $ putStrLn $ "    The identifiers "++ show atoms
                       io $ putStrLn $ "    have not been declared in a schema."
                Result_QueryFail BoundVarNotInBody -> do
                      io $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                      io $ putStrLn $ "    Variables bound by a lambda abstraction that do not appear"
                      io $ putStrLn $ "    In the body of a query."                    
                Result_QuerySuccess solutions -> do
                    case solutions of
                         [] -> do
                            io $ putStrLn (yellow colorOpts "No solutions.")
                         (x:[]) -> do
                            if (show x == "true")
                            then io $ putStrLn (green colorOpts "True.")
                            else return ()
                         _  -> do
                            io $ mapM_ (putStrLn . solutionToJson) solutions
                Result_AssertionSuccess -> do
                  io $ putStrLn $ (green colorOpts "OK.")++" Assertion successful."
                Result_AssertionFail atoms -> do
                       io $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                       io $ putStrLn $ "    The identifiers "++ show atoms
                       io $ putStrLn $ "    have not been declared in a schema."

-- | Main entrypoint for the bli-prolog REPL.
repl :: Bli ()
repl = do
  -- Just get colorOpts, version doesn't matter here.
  opts    <- getOpts
  clauses <- getProgram
  schema  <- getSchema
  let colorOpts = not $ nocolor opts
  maybeLine <- io $ readline (blue colorOpts "?- ")
  case maybeLine of
    Nothing -> repl
    Just line -> do
      case line of 
        -- First,handle user REPL commands (beginning with a semicolon).
        ":h"   -> do 
          io $ putStrLn $ replHelpScreen colorOpts
          repl
        ":exit" -> return ()
        _ | isPrefixOf ":load" line -> do
               let filePath = drop 6 line
               fileContents <- io $ readFile filePath
               case fileExtension filePath of
                 ".pl"   -> do
                     io $ putStrLn "Do the appropriate parsing."
                 ".bpl"  -> do
                     io $ putStrLn "Do the appropriate parsing."
                 ".bsch" -> do
                     io $ putStrLn "Do the appropriate parsing."
               -- Helper function to get the file extension of a file.
               -- io $ putStrLn $ yellow colorOpts "Load command not implemented."
               repl
          | isPrefixOf ":export" line -> do
               let filePath = drop 8 line
               case fileExtension filePath of
                 ".pl" -> do
                   let contents = undefined
                   io $ writeFile filePath contents
                 ".bpl"  -> do
                   let contents = undefined
                   io $ writeFile filePath contents
                 ".bsch" -> do
                   let contents = undefined
                   io $ writeFile filePath contents
               -- io $ putStrLn $ yellow colorOpts "Export command not implemented."
               repl
         | (line == ":lkb" || line == ":list-knowledge-base") -> do
               io $ mapM_ (\x -> putStrLn ("  "++x)) $ map prettyShowClause clauses
               repl
         | (line == ":ls" || line == ":list-schema") -> do
               io $ mapM_ (\x -> putStrLn ("  "++x)) $ map prettyShowSchemaEntry schema
               repl 
        -- If the user has not entered a REPL command, try processing
        -- their input as a standard Bedelibry Prolog command.
          | otherwise -> do
                processCliInput line
                repl