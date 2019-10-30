
--
-- | Logic for the command line interface of bli-prolog
-- 

module Bli.App.Cli where

import Control.Monad.Bli
import Control.Monad
import Control.Exception.Base
import qualified Control.Monad.Bli.Pure as Pure
import Bli.Util
import Bli.App
import Bli.App.Api
import Bli.App.Json
import Data.List.Split
import Bli.App.Colors
import Bli.App.Config
import Bli.App.Config.Data
import Bli.Prolog.Typechecking
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Data.Aeson hiding (json)
import Text.ParserCombinators.Parsec (parse)
import Bli.Prolog.Parser.Cli
import Bli.Prolog.Parser.Schema
import Bli.Prolog.Parser
import Bli.Prolog.Parser.Terms
import Bli.Prolog.Interp
import Bli.Prolog.Interp.Data
import Data.List
import Data.Alias (toKVList)
import Control.Empty
import System.Console.CmdArgs as CA hiding (program)
import System.Console.Readline
import Control.Monad.IO.Class

-- | Helper function to get the file extension of a filepath.
fileExtension :: String -> String
fileExtension filePath = "." ++ (last $ splitOn "." filePath)

-- | Helper function for formatting error messages dealing with n-ary predicates.
fmt :: Int -> String
fmt 0 = "null"
fmt 1 = "un"
fmt 2 = "bi"
fmt 3 = "tri"
fmt 4 = "quater"
fmt 5 = "quin"
fmt 6 = "sen"
fmt 7 = "septen"
fmt 8 = "octon"
fmt 9 = "noven"
fmt 10 = "den"
fmt n = (show n) ++ "-"

-- | Helper function to process a bli prolog command at the REPL.
processBliCommandRepl :: BliCommand -> Bli ()
processBliCommandRepl command = do
  -- Get schema, clauses, and options from context.
  types     <- getTypes
  relations <- getRelations
  entities  <- getEntities
  facts     <- getFacts
  opts      <- getConfig
  aliases   <- getAliases
  let colorOpts = not $ nocolor opts
  
  result <- processBliCommand command
  -- Handle the result of processing the command.
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
    Result_QueryFail_NotAPredicate ((id, n, typ):_) -> do
      printResponse $ (red colorOpts "Failure.")++" Query unsuccesful."
      printResponse $ "    Identifier "++id++" is being used as an "++fmt n++"ary predicate"
      printResponse $ "    but is declared to be a term of type "++typ++"."
    Result_QueryFail_TypeError _ -> do
      printResponse $ (red colorOpts "Failure.")++" Query unsuccesful."
      printResponse $ "    Type error."
    Result_QuerySuccess [ProcReturn] -> return ()
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
      -- Note: If we have stored somewhere a list of how to make a type into
      -- its plural form, then we can have better responses here.
    Result_AssertionSuccess_AddedEntityLocally entityName entityType  -> do
      printResponse $ (green colorOpts "Ok. ")++"Added "++entityName++" to the list of entities of type "++entityType++"."
    Result_AssertionSuccess_AddedEntityBedelibry entityName entityType -> do
      printResponse $ (green colorOpts "Ok.")
      printResponse $ "    Added "++entityName++" to the list of entities of type"
      printResponse $ "    "++entityType++" in the Bedelibry server."
    Result_AssertionFail_EntityNotDeclared t x -> do
      printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
      printResponse $ "    The term "++t++" has not been declared as an entity"
      printResponse $ "    of type "++x++"."
    Result_AssertionFail_TypeNotDeclared t -> do
      printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
      printResponse $ "    The type "++t++"\n    has not been declared."
    Result_AssertionFail_NotAPredicate ((id, n, typ):_) -> do
      printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
      printResponse $ "    Identifier "++id++" is being used as an "++fmt n++"ary predicate"
      printResponse $ "    but is declared to be a term of type "++typ++"."
    Result_AssertionFail_TypeError ((p, n, expectedType, actualType):_) -> do
      printResponse $ (red colorOpts "Failure.")++" Assertion unsuccesful."
      printResponse $ "    Type error. In predicate "++p++" at argument "++show n++","
      printResponse $ "    expected a term of type "++expectedType++" but instead recieved a term of type "++actualType++"."
    Result_AssertionFail_AtomsNotInSchema atoms -> do
      printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
      printResponse $ "    The identifiers "++ show atoms
      printResponse $ "    have not been declared in a schema."
    Result_AssertionFail_AlreadyAsserted -> do
      printResponse $ (yellow colorOpts "Already asserted.")
    Result_AssertionFail_CannotDeclareEntityOfBuiltinType str -> do
      printResponse $ (red colorOpts "Failure.")++ " Assertion unsuccessful."
      printResponse $ "    Cannot declare an entity of builtin type "++ str ++ "."
    Result_AssertionFail_CannotDeclaraDatatypeAsEntity -> do
      printResponse $ (red colorOpts "Failure.")++" Assertion unsuccessful."
      printResponse $ "    Cannot declare a datatype or data constructor as"
      printResponse $ "    an entity."

-- | Helper function to process bli-prolog commands in a running application.
processCliInput :: String -> Bli ()
-- The REPL should not produce any output for empty input.
processCliInput "" = return () 
processCliInput input = do
  -- Get schema, clauses, and options from context.
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
      processBliCommandRepl command

-- | Function used for tab completion in the REPL.
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
  let colorOpts = not $ nocolor config
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
                maybeFileContents <- do
                  -- Handle file read exceptions.
                  liftIO $ catch (Just <$> readFile filePath)
                                 (\e -> do
                                    printResponse $ (red colorOpts "Error") ++ " reading file " ++ filePath ++ ":"
                                    mapM_ (\x -> printResponse $ "  " ++ x) (splitOn "\n" (show (e :: IOException)))
                                    return Nothing)
                case maybeFileContents of
                  -- If there was an error parsing the file contents,
                  -- Continue with the REPl.
                  Nothing -> repl
                  Just fileContents -> do
                    case fileExtension filePath of
                       PlainPlExtension   -> do
                          case clausesFromString fileContents of
                              Left e -> printResponse "There has been a parse error."
                              Right clauses -> do
                                   printResponse "Need to implement the logic for adding clauses here."
                                  -- modifyProgram (\x -> x ++ clauses)
                       BliPlExtension  -> do
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
                       SchemaFileExtension -> do
                          case parseTypedSchema fileContents of
                              Left e -> printResponse "There has been a parse error."
                              Right entries -> do
                                    printResponse $ show entries
                                    printResponse "Need to implement the logic for modifying the schema here."
                                   -- modifySchema (\x -> x ++ (getArities entries))
                    repl
             ExportFile filePath -> do
                case fileExtension filePath of
                  PlainPlExtension -> do
                    let contents = undefined
                    liftIO $ catch (writeFile filePath contents)
                                   (\e -> do
                                      printResponse $ (red colorOpts "Error") ++ " writing file "++ filePath ++ ":"
                                      mapM_ (\x -> printResponse $ "  " ++ x) (splitOn "\n" (show (e :: IOException))))
                  BliPlExtension  -> do
                    let contents = undefined
                    liftIO $ catch (writeFile filePath contents)
                                   (\e -> do
                                      printResponse $ (red colorOpts "Error") ++ " writing file "++ filePath ++ ":"
                                      mapM_ (\x -> printResponse $ "  " ++ x) (splitOn "\n" (show (e :: IOException))))
                  SchemaFileExtension -> do
                    let contents = undefined
                    liftIO $ catch (writeFile filePath contents)
                                   (\e -> do
                                      printResponse $ (red colorOpts "Error") ++ " writing file "++ filePath ++ ":"
                                      mapM_ (\x -> printResponse $ "  " ++ x) (splitOn "\n" (show (e :: IOException))))
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
             GetTypeOf input -> do
               let maybeAtom = parse atomP "" input
               case maybeAtom of
                 Left e -> do 
                   printResponse $ show e
                   repl
                 Right atom -> do 
                   response <- typeOfAtom atom
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