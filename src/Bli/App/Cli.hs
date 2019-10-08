
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
import Control.Monad.IO.Class

-- Helper function to get the file extension of a filepath.
fileExtension :: String -> String
fileExtension filePath = "." ++ (last $ splitOn "." filePath)

groupSchemaClauses commands = go commands [] [] 
 where go [] ss cs = (ss,cs)
       go ((AssertClause c):xs) ss cs   = go xs ss (c:cs)
       go ((AssertTypePred s):xs) ss cs = go xs (s:ss) cs

-- | Helper function to process bli-prolog commands in a running application.
processCliInput :: String -> Bli ()
processCliInput input = do
          -- Get schema, clauses, and options from context.
          -- Temporary, to get this to compile.
          let schema = []
          types     <- getTypes
          relations <- getRelations
          entities  <- getEntities
          clauses   <- getFacts
          opts      <- getConfig
          let colorOpts = not $ nocolor opts
          -- Parse and handle the command
          let parserOutput = parseBliCommandTyped input
          case parserOutput of
            Left err -> do liftIO $ putStrLn ((red colorOpts "Error")++" parsing query string:")
                           liftIO $ putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
                           liftIO $ putStrLn $ 
                             (yellow colorOpts
                                "All bli prolog commands end with either a '.' or an '!'.")
            Right command -> do
              result <- Pure.liftFromPure $ processBliCommand command
              case result of
                Result_QueryFail (AtomsNotInSchema atoms) -> do
                       liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                       liftIO $ putStrLn $ "    The identifiers "++ show atoms
                       liftIO $ putStrLn $ "    have not been declared in a schema."
                Result_QueryFail BoundVarNotInBody -> do
                      liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                      liftIO $ putStrLn $ "    Variables bound by a lambda abstraction that do not appear"
                      liftIO $ putStrLn $ "    In the body of a query."                    
                Result_QueryFail_WrongArities xs -> do
                      liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                      liftIO $ mapM_ (\(id,arity) -> putStrLn $ "    The identifier "++id++
                            " exists in the schema, but should not be used with an arity of "++show arity ++".")
                            xs
                Result_QuerySuccess solutions -> do
                    case solutions of
                         [] -> do
                            liftIO $ putStrLn (yellow colorOpts "No solutions.")
                         (x:[]) -> do
                            if (show x == "true")
                            then liftIO $ putStrLn (green colorOpts "True.")
                            else return ()
                         _  -> do
                            liftIO $ mapM_ (putStrLn . solutionToJson) solutions
                Result_AssertionSuccess -> do
                  liftIO $ putStrLn $ (green colorOpts "OK.")++" Assertion successful."
                Result_AssertionFail atoms -> do
                       liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                       liftIO $ putStrLn $ "    The identifiers "++ show atoms
                       liftIO $ putStrLn $ "    have not been declared in a schema."
                Result_AssertionFail_WrongArities xs -> do
                      liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                      liftIO $ mapM_ (\(id,arity) -> putStrLn $ "    The identifier "++id++
                                  " exists in the schema, but should not be used with an arity of "
                                  ++ show arity ++".")
                                 xs
                Result_AssertionFail_AlreadyAsserted -> do
                    liftIO $ putStrLn $ (yellow colorOpts "Already asserted.")

-- | Main entrypoint for the bli-prolog REPL.
repl :: Bli ()
repl = do
  -- Just get colorOpts, version doesn't matter here.
  opts    <- getConfig
  clauses <- getFacts
  -- Temporary, to get this to compile
  let schema = [] 
  let colorOpts = not $ nocolor opts
  maybeLine <- liftIO $ readline (blue colorOpts command_prompt)
  case maybeLine of
    Nothing -> repl
    Just line -> do
      case line of 
        -- First,handle user REPL commands (beginning with a semicolon).
        ":h"   -> do 
          liftIO $ putStrLn $ replHelpScreen colorOpts
          repl
        ":clear-kb" -> do
            liftIO $ putStrLn "Clearing all in-memory facts."
            setFacts []
        ":clear-schema" -> do
            -- Note: This should also clear everything in the schema, since
            -- if have no entities which we can query about, then
            -- we also know no facts about those entities.
            liftIO $ putStrLn "Clearing all in-memory facts and schema data"
            setFacts []
            liftIO $ putStrLn "Warning: Need to update the logic here."
            -- setSchema  []
        ":exit" -> return ()
        _ | isPrefixOf ":load" line -> do
               let filePath = drop 6 line
               fileContents <- liftIO $ readFile filePath
               case fileExtension filePath of
                 ".pl"   -> do
                     case clausesFromString fileContents of
                         Left e -> liftIO $ putStrLn "There has been a parse error."
                         Right clauses -> do
                             liftIO $ putStrLn "Need to implement the logic for adding clauses here."
                             -- modifyProgram (\x -> x ++ clauses)
                 ".bpl"  -> do
                     -- Currently this will only parse the typed version
                     case parseTypedBli fileContents of
                         Left e -> liftIO $ putStrLn "There has been a parse error."
                         Right lines -> do
                             -- debugging
                             liftIO $ print $ lines
                             -- This is the old version.
                             -- let (entries, clauses) = groupSchemaClauses lines
                             -- modifyProgram (\x -> x ++ clauses)
                             -- modifySchema  (\x -> x ++ entries)
                 ".bsch" -> do
                  -- Currently this will only parse the typed version.
                     case parseTypedSchema fileContents of
                         Left e -> liftIO $ putStrLn "There has been a parse error."
                         Right entries -> do
                              liftIO $ print $ entries
                              liftIO $ putStrLn "Need to implement the logic for modifying the schema here."
                              -- modifySchema (\x -> x ++ (getArities entries))
               repl
          | isPrefixOf ":export" line -> do
               let filePath = drop 8 line
               case fileExtension filePath of
                 ".pl" -> do
                   let contents = undefined
                   liftIO $ writeFile filePath contents
                 ".bpl"  -> do
                   let contents = undefined
                   liftIO $ writeFile filePath contents
                 ".bsch" -> do
                   let contents = undefined
                   liftIO $ writeFile filePath contents
               -- io $ putStrLn $ yellow colorOpts "Export command not implemented."
               repl
         | isPrefixOf ":alias" line -> do
             let args' = drop 7 line
             let args = words $ args'
             if (length args == 2)
             then do
               let arg1 = args !! 0
               let arg2 = args !! 1
               liftIO $ putStrLn $ "Made alias of " ++ arg1 ++ " to " ++ arg2 ++ "."
               repl
             else do
               liftIO $ putStrLn "Invalid argument format to :alias."
               repl
         | (line == ":lkb" || line == ":list-knowledge-base") -> do
               liftIO $ mapM_ (\x -> putStrLn ("  "++x)) $ map prettyShowClause clauses
               repl
         | (line == ":ls" || line == ":list-schema") -> do
               liftIO $ putStrLn "Warning: The logic here needs to be changed."
               -- liftIO $ mapM_ (\x -> putStrLn ("  "++x)) $ map prettyShowSchemaEntry schema
               repl 
        -- If the user has not entered a REPL command, try processing
        -- their input as a standard Bedelibry Prolog command.
          | otherwise -> do
                processCliInput line
                repl