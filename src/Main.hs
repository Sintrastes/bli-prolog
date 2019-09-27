{-# LANGUAGE DeriveDataTypeable #-}

--
-- | Main entrypoint for the bli-prolog executable.
--

module Main where

import Data.Prolog.Ast
import Data.Schema
import Schema.Parser
import qualified Prolog.Parser as P
import qualified Prolog.Interp as I
import qualified Prolog.Analysis as A
import Bli.App.Config
import Control.Monad.Bli

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
    Nothing -> putStrLn $ "\27[31m"++"Error loading version info from cabal file. Aborting."++"\27[37m"
    Just version -> do
      opts <- cmdArgs $ startOptions version
      -- If prolog file not specified, start with an empty set of clauses.
      p <- case program opts of
        "" -> return $ Right []
        _  -> P.clausesFromFile $ program opts
      -- If Schema file not specified, start with an empty schema. 
      s <- case schema opts of
        "" -> return $ Right []
        _  -> schemaFromFile $ schema opts
      -- Handle parse errors for prolog and schema files.
      case (p,s) of 
        (Left err,_) -> do putStrLn ("\27[31m"++"Error"++"\27[37m"++" parsing prolog file:") 
                           putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
        (_,Left err) -> do putStrLn ("\27[31m"++"Error"++"\27[37m"++" parsing schema file:") 
                           putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
        -- If all files parse sucessfully...
        (Right clauses, Right schema) ->
          case goal opts of
            "" -> do
               -- Print the main banner if options set to verbose.
               if (verbose opts) then putStrLn $ replBanner version else return ()
               -- Run a bli prolog REPL with the user configuration.
               runBli opts clauses schema $ repl
            -- If the user supplies a non-empty goal-string, run a single
            -- command rather than starting the REPL.
            input -> runBli opts clauses schema $ processBliCommand input

repl :: Bli ()
repl = do
  maybeLine <- io $ readline ("\27[36m"++"?- "++ "\27[37m")
  case maybeLine of
    Nothing -> repl
    Just line -> do
      case line of 
        -- First,handle user REPL commands (beginning with a semicolon).
        ":h"   -> do 
          io $ putStrLn replHelpScreen
          repl
        ":exit" -> return ()
        _ | isPrefixOf ":load" line -> do
               io $ putStrLn "\27[33mLoad command not implemented.\27[37m"
               repl
          | isPrefixOf ":export" line -> do
               io $ putStrLn "\27[33mExport command not implemented.\27[37m"
               repl
        -- If the user has not entered a REPL command, try processing
        -- their input as a standard Bedelibry Prolog command.
          | otherwise -> do
                processBliCommand line
                repl

processBliCommand :: String -> Bli ()
processBliCommand input' = do
          -- Get schema, clauses, and options from context.
          schema <- getSchema
          clauses <- getProgram
          opts <- getOpts
          -- Prepend the user input with the required symbol for
          -- our parsers
          let input = "?- " ++ input'
          -- Parse and handle the command
          let command = P.parseBliCommand input
          case command of   
              Right x@(AssertMode goal) -> do
                   case A.isBliCommandValid x schema of
                     Right A.Ok -> do
                       io $ putStrLn $ "\27[32m"++"OK."++"\27[37m"++" Assertion successful."
                       modifyProgram (\clauses -> clauses ++ (map (\term -> (term,[])) goal))
                     Left (A.AtomsNotInSchema atoms) -> do
                       io $ putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Assertion unsuccessful."
                       io $ putStrLn $ "    The identifiers "++ show atoms
                       io $ putStrLn $ "    have not been declared in a schema."
              Right x@(AssertClause clause) -> do
                   case A.isBliCommandValid x schema of
                     Right A.Ok -> do
                       io $ putStrLn $ "\27[32m"++"OK."++"\27[37m"++" Assertion successful."
                       modifyProgram (\clauses -> clauses ++ [clause])
                     Left (A.AtomsNotInSchema atoms) -> do
                       io $ putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Assertion unsuccessful."
                       io $ putStrLn $ "    The identifiers "++ show atoms
                       io $ putStrLn $ "    have not been declared in a schema."
              Right x@(LambdaQuery (vars, goal)) -> do
                  case A.isBliCommandValid x schema of
                    Right A.Ok -> do
                      let t = I.makeReportTree clauses goal
                      io $ print $ map I.Solution 
                                 $ map (filter (\(x,y) -> x `elem` vars)) 
                                 -- Note: This is currently fixed to use bfs.
                                 $ map (\(I.Solution x) -> x) $ I.bfs t
                    Left (A.AtomsNotInSchema atoms) -> do
                      io $ putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Query unsuccessful."
                      io $ putStrLn $ "    The identifiers "++ show atoms
                      io $ putStrLn $ "    have not been declared in a schema."
                    Left (A.BoundVarNotInBody) -> do
                      io $ putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Query unsuccessful."
                      io $ putStrLn $ "    Variables bound by a lambda abstraction that do not appear"
                      io $ putStrLn $ "    In the body of a query."
              Right x@(QueryMode goal) -> do
                   case A.isBliCommandValid x schema of
                     Right A.Ok -> do
                       let limiting lst = case limit opts of
                             Nothing -> lst
                             Just n  -> take n lst
                       let searchF = searchFunction (search opts) $ depth opts
                       let t = I.makeReportTree clauses goal
                       let solutions = limiting $ searchF t
                       case solutions of
                         [] -> do
                            io $ putStrLn ("\27[33m"++"No solutions."++"\27[37")
                         (x:[]) -> do
                            if (show x == "true")
                            then io $ putStrLn ("\27[32m"++"True."++"\27[37")
                            else return ()
                         _  -> do
                            io $ mapM_ print solutions
                     Left (A.AtomsNotInSchema atoms) -> do
                      io $ putStrLn $ "\27[31m"++"Failure."++"\27[37m"++" Query unsuccessful."
                      io $ putStrLn $ "    The identifiers "++ show atoms
                      io $ putStrLn $ "    have not been declared in a schema."
                   -- This case should not be possible since we are not dealing with a
                   -- lambda query.
                     Left _ -> do
                       io $ error $ "Invalid exception encountered."
              Left err -> do io $ putStrLn ("\27[31m"++"Error"++"\27[37m"++" parsing query string:")
                             io $ putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                               (map (\x -> "  " ++ x)) $ 
                                               (splitOn "\n" $ show err)
                             io $ putStrLn $ 
                               "\27[33m" ++ 
                                  "All bli prolog commands end with either a '.' or an '!'."
                                ++ "\27[37m"
