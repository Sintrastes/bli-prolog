
--
-- | Logic for the command line interface of bli-prolog
-- 

module Bli.App.Cli where

import Control.Monad.Bli
import Control.Monad
import qualified Control.Monad.Bli.Pure as Pure
import Bli.App
import Bli.App.Api
import Bli.App.Json
import Data.List.Split
import Bli.App.Colors
import Bli.App.Config
import Bli.Prolog.Typechecking
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Data.Aeson hiding (json)
import Bli.Prolog.Parser.Cli
import Bli.Prolog.Parser.Schema
import Bli.Prolog.Parser
import Bli.Prolog.Interp
import Data.List
import Data.Alias (toKVList)
import Control.Empty
import System.Console.CmdArgs as CA hiding (program)
import System.Console.Readline
import Control.Monad.IO.Class

-- Helper function to get the file extension of a filepath.
fileExtension :: String -> String
fileExtension filePath = "." ++ (last $ splitOn "." filePath)

-- (types, relations, entities, clauses)
groupSchemaClauses :: [BliCommandTyped] -> ([TypeDecl], [RelDecl], [EntityDecl], [Clause])
groupSchemaClauses commands = go commands ([], [], [], []) 
 where go [] xs = xs
       go ((T_AssertClause c):xs) (types,relations,entities,clauses)
           = go xs (types, relations, entities, c:clauses)
       go ((T_AssertSchema (Type t)):xs) (types,relations,entities,clauses)
           = go xs (t:types, relations, entities, clauses)
       go ((T_AssertSchema (TypeOf t ty)):xs) (types,relations,entities,clauses)
           = go xs (types, relations, (t,ty):entities, clauses)
       go ((T_AssertSchema (Pred isStored name argTypes dirs)):xs) (types,relations,entities,clauses)
           = go xs (types, (name, argTypes):relations, entities, clauses)

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
            Left err -> do printResponse $ ((red colorOpts "Error")++" parsing query string:")
                           printResponse $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                             (map (\x -> "  " ++ x)) $ 
                                             (splitOn "\n" $ show err)
                           printResponse $ 
                             (yellow colorOpts
                                "All bli prolog commands end with either a '.' or an '!'.")
            Right command -> do
              result <-  processBliCommand command
              case result of
                Result_QueryFail_AtomsNotInSchema atoms -> do
                  printResponse $ (red colorOpts "Failure.")++" Query unsuccessful."
                  printResponse $ "    The identifiers "++ show atoms
                  printResponse $ "    have not been declared in a schema."
                Result_QueryFail_BoundVarNotInBody -> do
                  printResponse $ (red colorOpts "Failure.")++" Query unsuccessful."
                  printResponse $ "    Variables bound by a lambda abstraction that do not appear"
                  printResponse $ "    In the body of a query."
                Result_QueryFail_EntityNotDeclared t x -> do
                  printResponse $ (red colorOpts "Failure.")++" Query unsuccessful."
                  printResponse $ "    The term "++t++" has not been declared as an entity"
                  printResponse $ "    of type "++x++"."
                Result_QueryFail_TypeNotDeclared t -> do
                  printResponse $ (red colorOpts "Failure.")++" Query unsuccessful."
                  printResponse $ "    The type "++t++"\n    has not been declared."
                Result_QueryFail_NotAPredicate _ -> do
                  printResponse $ (red colorOpts "Failure.")++" Assertion unsuccesful."
                  printResponse $ "    Not a predicate."
                Result_QueryFail_TypeError _ -> do
                  printResponse $ (red colorOpts "Failure.")++" Query unsuccesful."
                  printResponse $ "    Type error."
                Result_QuerySuccess solutions -> do
                    case solutions of
                         [] -> do
                            printResponse (yellow colorOpts "No solutions.")
                         (x:[]) -> do
                            if (show x == "true")
                            then printResponse (green colorOpts "True.")
                            else case json opts of
                                   True  -> liftIO $ mapM_ (putStrLn . solutionToJson) solutions
                                   False -> liftIO $ mapM_ print solutions
                         _  -> do
                            case json opts of
                              True  -> liftIO $ mapM_ (putStrLn . solutionToJson) solutions
                              False -> liftIO $ mapM_ print solutions 
                Result_AssertionSuccess -> do
                  printResponse $ (green colorOpts "OK.")++" Assertion successful."
                Result_AssertionFail_EntityNotDeclared t x -> do
                  printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                  printResponse $ "    The term "++t++" has not been declared as an entity"
                  printResponse $ "    of type "++x++"."
                Result_AssertionFail_TypeNotDeclared t -> do
                  printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                  printResponse $ "    The type "++t++"\n    has not been declared."
                Result_AssertionFail_NotAPredicate _ -> do
                  printResponse $ (red colorOpts "Failure.")++" Assertion unsuccesful."
                  printResponse $ "    Not a predicate."
                Result_AssertionFail_TypeError _ -> do
                  printResponse $ (red colorOpts "Failure.")++" Assertion unsuccesful."
                  printResponse $ "    Type error."
                Result_AssertionFail_AtomsNotInSchema atoms -> do
                  printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
                  printResponse $ "    The identifiers "++ show atoms
                  printResponse $ "    have not been declared in a schema."
                Result_AssertionFail_AlreadyAsserted -> do
                  printResponse $ (yellow colorOpts "Already asserted.")

-- | Function used to use for tab completion in the REPL.
completionFunction :: String -> IO [String]
completionFunction x = 
   if any (\y -> isPrefixOf x y) commandStrings
   then return $ filter (isPrefixOf x) commandStrings
   else return []
 where commandStrings = join $ map bliReplCommandStrings [minBound..maxBound]

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
  liftIO $ setCompletionEntryFunction $ Just completionFunction
  maybeLine <- liftIO $ readline (blue colorOpts command_prompt)
  case maybeLine of
    Nothing -> repl
    Just line -> do
      -- Add the user's input to the command line history.
      liftIO $ addHistory line
      case parseBliReplCommand line of 
        ParseError err -> do 
            printResponse "There was an error parsing the command:"
            printResponse $ "  " ++ show err
        DoneParsing blicmd ->
          case blicmd of
             Help -> do
               screen <- liftIO $ replHelpScreen colorOpts
               printResponse screen
               repl
             Exit -> do
               return ()
             ClearSchema -> do
               printResponse "Clearing all in-memory schema data."
               setFacts empty
               setRelations empty
               setEntities empty
               repl
             ClearRelations -> do
               printResponse "Clearing all in-memory relations (and facts)."
               setRelations empty
               setFacts empty
               repl
             ClearEntities -> do
               printResponse "Clearing all in-memory entities (and facts)."
               setEntities  empty
               setRelations empty
               repl
             ClearFacts -> do
               printResponse "Clearing all in-memory facts."
               setFacts empty
               repl
             ListSchema ->
               if (entities  == empty
                 && relations == empty
                 && types     == empty)
               then do 
                 printResponse $ yellow colorOpts "Schema is empty."
                 repl
               else do
                 printResponse "NOT IMPLEMENTED."
                 repl
             ListRelations ->
               if relations == empty
               then do 
                 printResponse $ yellow colorOpts "No relations in store."
                 repl
               else do
                 liftIO $ mapM_ printResponse $ map (\(name, types) -> "rel "++name++": " ++ intercalate ", " types ++ "." ) $ relations
                 repl
             ListTypes ->
               if types == empty
               then do 
                 printResponse $ yellow colorOpts "No types in store."
                 repl
               else do
                 mapM_ (printResponse . (\x -> x ++ ".")) types
                 repl
             ListEntities ->
               if entities == empty
               then do
                 printResponse $ yellow colorOpts "No entities in store."
                 repl
               else do
                 mapM_ (printResponse . (\(x,typ) -> x ++ ": " ++ typ ++ "." )) entities
                 repl
             ListFacts ->
               if facts == empty
               then do
                 printResponse $ yellow colorOpts "No facts in store."
                 repl
               else do
                 mapM_ printResponse $ map prettyShowClause facts
                 repl
             ListAliases -> do
               if aliases == empty
               then do 
                 printResponse $ yellow colorOpts "No aliases in store."
                 repl
               else do 
                 -- Note: To make this work, I probably need a "to list" function
                 -- for aliases, so that I can get this to print properly.
                 mapM_ printResponse $ map (\(id1, id2) -> "alias " ++ id1 ++ " " ++ id2 ++ ".")  $ toKVList aliases
                 repl
             LoadFile filePath -> do
                fileContents <- liftIO $ readFile filePath
                case fileExtension filePath of
                  ".pl"   -> do
                      case clausesFromString fileContents of
                          Left e -> printResponse "There has been a parse error."
                          Right clauses -> do
                               printResponse "Need to implement the logic for adding clauses here."
                              -- modifyProgram (\x -> x ++ clauses)
                  ".bpl"  -> do
                      -- Currently this will only parse the typed version
                      case parseTypedBli fileContents of
                          Left e -> printResponse "There has been a parse error."
                          Right lines -> do
                              -- Note: We still need to do typechecking of the file here!
                              let (types, relations, entities, clauses) = groupSchemaClauses lines
                              newEntities entities
                              newFacts clauses
                              newTypes types
                              newRelations relations
                              return ()
                  ".bsc" -> do
                      case parseTypedSchema fileContents of
                          Left e -> printResponse "There has been a parse error."
                          Right entries -> do
                               printResponse $ show entries
                               printResponse "Need to implement the logic for modifying the schema here."
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
                   SuccessfullyAdded -> do
                     printResponse $ "Made alias of " ++ arg1 ++ " to " ++ arg2 ++ "."
                     repl
                   AliasAlreadyInStore -> do
                     printResponse $ "Failure. Alias is already is store."
                     repl
                   DoesNotHavePrimaryIDOrAlias -> do
                     printResponse $ "Failure. Neither " ++ arg1 ++ " nor " ++ arg2 ++ " are a primary ID"
                     printResponse $ "Or a pre-existing alias of a primary ID."
                     repl
             GetTypeOf t -> do
               -- TODO: Actually parse this into an atom here.
               response <- typeOfAtom (Identifier t)
               case response of
                 Nothing -> printResponse "Did not typecheck"
                 Just typ -> printResponse $ show typ
               repl
             ShowPort -> do
                liftIO $ print (port config)
                repl
             GetPID id -> do
                pid' <- lookupPrimaryID id
                case pid' of
                  Just pid -> do
                    printResponse $ pid
                  Nothing -> do
                    printResponse $ "The term " ++ show id ++ " does not have a primary ID."
                repl
        -- If the user has not entered a REPL command, try processing
        -- their input as a standard Bedelibry Prolog command.
        ContinueParsing -> do
          processCliInput line
          repl