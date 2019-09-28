{-# LANGUAGE DeriveDataTypeable #-}

--
-- | Main entrypoint for the bli-prolog executable.
--

module Main where

import Data.Prolog.Ast
import Data.Schema
import Schema.Parser
import Prolog.Parser
import Prolog.Interp
import Prolog.Analysis
import Bli.App.Config
import Bli.App.Colors
import Bli.App.Json
import Bli.App.Server
import Bli.App.Api
import Control.Monad.Bli
import qualified Control.Monad.Bli.Pure as Pure

import Control.Monad (when)
import Data.List (intersperse, isPrefixOf)
import System.Console.Readline
import Data.List.Split
import System.Console.CmdArgs as CA hiding (program)

main = do
  -- Get the version from the cabal file at compile-time.
  let v = $(getVersionFromCabal)
  -- Make sure version loaded from file successfully.
  case v of 
    Nothing -> putStrLn $ (red True) "Error loading version info from cabal file. Aborting."
    Just version -> do
      opts <- cmdArgs $ startOptions version
      let colorOpts = not $ nocolor opts
      -- If prolog file not specified, start with an empty set of clauses.
      p <- case program opts of
        "" -> return $ Right []
        _  -> clausesFromFile $ program opts
      -- If Schema file not specified, start with an empty schema. 
      s <- case schema opts of
        "" -> return $ Right []
        _  -> schemaFromFile $ schema opts
      -- Handle parse errors for prolog and schema files.
      case (p,s) of 
        (Left err,_) -> do putStrLn ((red colorOpts "Error") ++ " parsing prolog file:") 
                           putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
        (_,Left err) -> do putStrLn ((red colorOpts "Error")++" parsing schema file:") 
                           putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
        -- If all files parse sucessfully...
        (Right clauses, Right schema) ->
          case server opts of
        -- Launch server
            True -> do
              case (port opts) of
                Just n -> runBli opts clauses schema (newServer n)
                Nothing -> putStrLn "Please specify a port number to use the server."
        -- If not configured to start server...
            False -> do
              case goal opts of
                "" -> do
                   -- Print the main banner if options set to verbose.
                   if (verbose opts) then putStrLn $ replBanner version colorOpts else return ()
                   -- Run a bli prolog REPL with the user configuration.
                   runBli opts clauses schema $ repl
                -- If the user supplies a non-empty goal-string, run a single
                -- command rather than starting the REPL.
                input -> runBli opts clauses schema $ processCliInput input

-- | Main entrypoint for the bli-prolog REPL.
repl :: Bli ()
repl = do
  -- Just get colorOpts, version doesn't matter here.
  opts <- getOpts
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
               io $ putStrLn $ yellow colorOpts "Load command not implemented."
               repl
          | isPrefixOf ":export" line -> do
               io $ putStrLn $ yellow colorOpts "Export command not implemented."
               repl
        -- If the user has not entered a REPL command, try processing
        -- their input as a standard Bedelibry Prolog command.
          | otherwise -> do
                processCliInput line
                repl

-- | New helper function for our refactoring
--   Note: To ensure for a consistent API
--   and to best allow reuse between
--   the cli and server interfaces, this
--   function should also update the state of
--   the running Bli instance.
processBliCommand :: BliCommand -> Pure.Bli BliResult
processBliCommand x = do
  opts <- Pure.getOpts
  clauses <- Pure.getProgram
  schema  <- Pure.getSchema
  case x of
    (AssertMode goal) -> do
         case isBliCommandValid x schema of
           Right Ok -> do
               Pure.modifyProgram 
                (\clauses -> clauses ++
                   (map (\term -> (term,[])) goal))
               return $ Result_AssertionSuccess
           Left (AtomsNotInSchema atoms) ->
               return $ Result_AssertionFail atoms
    (AssertClause clause) -> do
         case isBliCommandValid x schema of
           Right Ok -> do
               Pure.modifyProgram (\clauses -> clauses ++ [clause]) 
               return $ Result_AssertionSuccess
           Left (AtomsNotInSchema atoms) ->
               return $ Result_AssertionFail atoms
    (LambdaQuery (vars, goal)) -> do
        case isBliCommandValid x schema of
          Right Ok -> 
            let t = makeReportTree clauses goal in
              return $ Result_QuerySuccess $ 
                           map Solution 
                         $ map (filter (\(x,y) -> x `elem` vars)) 
                         -- Note: This is currently fixed to use bfs.
                         $ map (\(Solution x) -> x) $ bfs t
          Left BoundVarNotInBody ->
            return $ Result_QueryFail BoundVarNotInBody
          Left (AtomsNotInSchema atoms) ->
            return $ Result_QueryFail (AtomsNotInSchema atoms)
    (QueryMode goal) ->
        case isBliCommandValid x schema of
          Right Ok ->
            return $ Result_QuerySuccess (  
              let limiting lst = case limit opts of
                    Nothing -> lst
                    Just n  -> take n lst
                  searchF = searchFunction (search opts) $ depth opts
                  t = makeReportTree clauses goal
                  solutions = limiting $ searchF t
              in solutions )
          Left (AtomsNotInSchema atoms) ->
            return $ Result_QueryFail (AtomsNotInSchema atoms)
    -- This case should not be possible since we are not dealing with a
    -- lambda query.
          Left _ -> error $ "Invalid exception encountered."

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