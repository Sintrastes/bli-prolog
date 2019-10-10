
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
import Control.Empty
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
          facts     <- getFacts
          opts      <- getConfig
          aliases   <- getAliases
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
                Result_QueryFail_AtomsNotInSchema atoms -> do
                       liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                       liftIO $ putStrLn $ "    The identifiers "++ show atoms
                       liftIO $ putStrLn $ "    have not been declared in a schema."
                Result_QueryFail_BoundVarNotInBody -> do
                      liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                      liftIO $ putStrLn $ "    Variables bound by a lambda abstraction that do not appear"
                      liftIO $ putStrLn $ "    In the body of a query."
                Result_QueryFail_EntityNotDeclared t x -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                  liftIO $ putStrLn $ "    The term "++t++" has not been declared as an entity"
                  liftIO $ putStrLn $ "    of type "++x++"."
                Result_QueryFail_TypeNotDeclared t -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccessful."
                  liftIO $ putStrLn $ "    The type "++t++"\n    has not been declared."
                Result_QueryFail_NotAPredicate _ -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccesful."
                  liftIO $ putStrLn $ "    Not a predicate."
                Result_QueryFail_TypeError _ -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Query unsuccesful."
                  liftIO $ putStrLn $ "    Type error."
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
                Result_AssertionFail_EntityNotDeclared t x -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                  liftIO $ putStrLn $ "    The term "++t++" has not been declared as an entity"
                  liftIO $ putStrLn $ "    of type "++x++"."
                Result_AssertionFail_TypeNotDeclared t -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                  liftIO $ putStrLn $ "    The type "++t++"\n    has not been declared."
                Result_AssertionFail_NotAPredicate _ -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccesful."
                  liftIO $ putStrLn $ "    Not a predicate."
                Result_AssertionFail_TypeError _ -> do
                  liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccesful."
                  liftIO $ putStrLn $ "    Type error."
                Result_AssertionFail_AtomsNotInSchema atoms -> do
                       liftIO $ putStrLn $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                       liftIO $ putStrLn $ "    The identifiers "++ show atoms
                       liftIO $ putStrLn $ "    have not been declared in a schema."
                Result_AssertionFail_AlreadyAsserted -> do
                    liftIO $ putStrLn $ (yellow colorOpts "Already asserted.")

-- | Main entrypoint for the bli-prolog REPL.
repl :: Bli ()
repl = do
  config    <- getConfig
  facts     <- getFacts
  types     <- getTypes
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  -- Temporary, to get this to compile
  let schema = [] 
  let colorOpts = not $ nocolor config
  -- Testing to see how this works.
  liftIO $ setCompletionEntryFunction $ Just (\x -> if x == "tes" then return ["test"] else return [])
  maybeLine <- liftIO $ readline (blue colorOpts command_prompt)
  case maybeLine of
    Nothing -> repl
    Just line -> do
      -- Add the user's input to the command line history.
      liftIO $ addHistory line
      case parseBliReplCommand line of 
        ParseError err -> do 
            liftIO $ putStrLn "There was an error parsing the command:"
            liftIO $ putStrLn $ "  " ++ show err
        DoneParsing blicmd ->
          case blicmd of
             Help -> do
               screen <- liftIO $ replHelpScreen colorOpts
               liftIO $ putStrLn screen
               repl
             Exit -> do
               return ()
             ClearSchema -> do
               liftIO $ putStrLn "Clearing all in-memory schema data."
               setFacts empty
               setRelations empty
               setEntities empty
               repl
             ClearRelations -> do
               liftIO $ putStrLn "Clearing all in-memory relations (and facts)."
               setRelations empty
               setFacts empty
               repl
             ClearEntities -> do
               liftIO $ putStrLn "Clearing all in-memory entities (and facts)."
               setEntities  empty
               setRelations empty
               repl
             ClearFacts -> do
               liftIO $ putStrLn "Clearing all in-memory facts."
               setFacts empty
               repl
             ListSchema ->
               if (entities  == empty
                 && relations == empty
                 && types     == empty)
               then do 
                 liftIO $ putStrLn $ yellow colorOpts "Schema is empty."
                 repl
               else do
                 liftIO $ putStrLn "NOT IMPLEMENTED."
                 repl
             ListRelations ->
               if relations == empty
               then do 
                 liftIO $ putStrLn $ yellow colorOpts "No relations in store."
                 repl
               else do
                 liftIO $ mapM_ print relations
                 repl
             ListTypes ->
               if types == empty
               then do 
                 liftIO $ putStrLn $ yellow colorOpts "No types in store."
                 repl
               else do
                 liftIO $ mapM_ print types
                 repl
             ListEntities ->
               if entities == empty
               then do
                 liftIO $ putStrLn $ yellow colorOpts "No entities in store."
                 repl
               else do
                 liftIO $ mapM_ print entities
                 repl
             ListFacts ->
               if facts == empty
               then do
                 liftIO $ putStrLn $ yellow colorOpts "No facts in store."
                 repl
               else do
                 liftIO $ mapM_ (\x -> putStrLn ("  "++x)) $ map prettyShowClause facts
                 repl
             ListAliases -> do
               if aliases == empty
               then do 
                 liftIO $ putStrLn $ yellow colorOpts "No aliases in store."
                 repl
               else do 
                 liftIO $ mapM_ print aliases
                 repl
             LoadFile filePath -> do
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
             ExportFile filePath -> do
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
             Alias arg1 arg2 -> do
                 -- Note: First should check here that 
                 -- the arguments parse properly as bli identifiers.
                 addedSuccessfully <- newAlias arg1 arg2
                 case addedSuccessfully of
                   True -> do
                     liftIO $ putStrLn $ "Made alias of " ++ arg1 ++ " to " ++ arg2 ++ "."
                     repl
                   False -> do
                     liftIO $ putStrLn $ "Failure. Alias is already is store."
                     repl
        -- If the user has not entered a REPL command, try processing
        -- their input as a standard Bedelibry Prolog command.
        ContinueParsing -> do
          processCliInput line
          repl