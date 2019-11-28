
--
-- | Logic for the command line interface of bli-prolog
-- 

module Bli.App.Cli where

import Data.Convert
import Control.Monad.Bli.Conversions (liftIORefFromPure)
import Control.Monad.Bli.IORef
import Control.Monad
import Control.Exception.Base hiding (TypeError)
import qualified Control.Monad.Bli.Pure as Pure
import Bli.Util
import Bli.App
import Bli.App.Formatting (printResponse)
import Bli.App.Api
import Bli.App.Json
import Data.List.Split
import Bli.App.Colors
import Bli.App.Config
import Bli.App.Config.Data
import Bli.App.Config.Features
import Bli.Prolog.Typechecking hiding (InvalidClause(..))
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
-- import System.Console.Readline
import Control.Monad.IO.Class
import Data.BliParser
import System.Console.Haskeline hiding (catch)
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Lazy.UTF8
import Control.Lens
import Network.Wreq hiding (get)
import qualified Network.Wreq as Wreq

import qualified Data.Text as Text

import Data.FuzzySet

liftFromPure = liftIORefFromPure

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
  results <- processBliCommand command
  mapM_ displayResult results

processBliCommandRemote :: String -> Bli ()
processBliCommandRemote input = do
  opts <- getConfig
  let Just address = remote opts
  -- Make an http request to the configured server, and 
  -- print the results
  response <- liftIO $ Wreq.get $ address ++ "/?query=" ++ input
  let Just result = read <$> toString <$> (response ^? responseBody) :: Maybe BliResult
  displayResult result

displayFailure :: FailureMode -> String
displayFailure (AtomsNotInSchema atoms) =
    "    The identifiers "++ show atoms++"\n"++
    "    have not been declared in a schema."
displayFailure BoundVarNotInBody =
    "    Variables bound by a lambda abstraction that do not appear\n"++
    "    In the body of a query."
displayFailure (EntityNotDeclared t x) =
    "    The term "++t++" has not been declared as an entity\n"++
    "    of type "++x++"."
displayFailure (TypeNotDeclared t) =
    "    The type "++t++"\n    has not been declared."
displayFailure (NotAPredicate ((id, n, typ):_)) =
    "    Identifier "++id++" is being used as an "++fmt n++"ary predicate\n"++
    "    but is declared to be a term of type "++typ++"."
displayFailure (TypeError ((p, n, expectedType, actualType):_)) =
  "    Type error. In predicate "++p++" at argument "++show n++",\n"++
  "    expected a term of type "++expectedType++" but instead recieved a term of type "++actualType++"."
displayFailure AlreadyAsserted = "  Already asserted."
displayFailure (CannotDeclareEntityOfBuiltinType str) =
  "    Cannot declare an entity of builtin type "++ str ++ "."
displayFailure CannotDeclaraDatatypeAsEntity =
  "    Cannot declare a datatype or data constructor as\n" ++
  "    an entity."

-- | Displays a single BliResult, formatted for the Command Line Interface.
displayResult :: BliResult -> Bli ()
displayResult result = do
  opts      <- getConfig
  let colorOpts = not $ nocolor opts
  case result of
    SyntaxError err -> do
      printResponse "Syntax error."
    ExtensionNotEnabled option -> do
      printResponse $ "The extension "++ show option++" is not enabled."
    QueryFail failureType -> do
      printResponse $ (red colorOpts "Failure.") ++
        " Query unsuccessful.\n" ++ displayFailure failureType
    AssertionFail failureType -> do
      printResponse $ (red colorOpts "Failure.") ++ 
        " Assertion unsuccessful.\n" ++ displayFailure failureType
    AssertionSuccess (AddedEntityLocally entityName entityType) -> do 
      printResponse $ (green colorOpts "Ok. ")++"Added "++entityName++" to the list of entities of type "++entityType++"."
    AssertionSuccess (AddedEntityBedelibry entityName entityType) -> do
      printResponse $ (green colorOpts "Ok.") ++ "\n" ++
        "    Added "++entityName++" to the list of entities of type\n" ++
        "    "++entityType++" in the Bedelibry server.\n"
    AssertionSuccess GenericAssertionSuccess -> do
      printResponse $ (green colorOpts "OK.")++" Assertion successful."
    QuerySuccess (QueryFinished [ProcReturn]) -> return ()
    QuerySuccess (QueryFinished solutions) -> do
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
  parserOutput <- liftFromPure $ parseBliCommandTyped input
  case parserOutput of
    Left err -> do printResponse $ ((red colorOpts "Error")++" parsing query string:")
                   printResponse $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                          (map (\x -> "  " ++ x)) $ 
                                          (splitOn "\n" $ show err)
                   printResponse $ 
                     (yellow colorOpts
                        "All bli prolog commands end with either a '.' or an '!'.")
    Right command -> do
      processBliCommandRemote input

processThinClientInput :: String -> Bli ()
processThinClientInput "" = return ()
processThinClientInput input = do
  -- Get schema, clauses, and options from context.
  types     <- getTypes
  relations <- getRelations
  entities  <- getEntities
  facts     <- getFacts
  opts      <- getConfig
  aliases   <- getAliases
  let colorOpts = not $ nocolor opts

  -- Parse and handle the command
  parserOutput <- liftFromPure $ parseBliCommandTyped input
  case parserOutput of
    Left err -> do printResponse $ ((red colorOpts "Error")++" parsing query string:")
                   printResponse $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                                          (map (\x -> "  " ++ x)) $ 
                                          (splitOn "\n" $ show err)
                   printResponse $ 
                     (yellow colorOpts
                        "All bli prolog commands end with either a '.' or an '!'.")
    Right command -> do
      processBliCommandRemote input

-- | Function used for tab completion in the REPL.
{-
completionFunction :: String -> IO [String]
completionFunction x = 
   if any (\y -> isPrefixOf x y) commandStrings
   then return $ filter (isPrefixOf x) commandStrings
   else return []
 where commandStrings = join $ map bliReplCommandStrings [minBound..maxBound]
-}

-- | Main entrypoint for the bli-prolog REPL.
repl :: Bli ()
repl = runInputT defaultSettings loop
  where loop = do
          config    <- lift $ getConfig
          facts     <- lift $ getFacts
          types     <- lift $ getTypes
          relations <- lift $ getRelations
          entities  <- lift $ getEntities
          aliases   <- lift$ getAliases
          let colorOpts = not $ nocolor config
          -- liftIO $ setCompletionEntryFunction $ Just completionFunction
          maybeLine <- getInputLine (blue colorOpts command_prompt)
          case maybeLine of
            Nothing -> loop
            Just line -> do
              -- Add the user's input to the command line history.
              -- liftIO $ addHistory line
              case parseBliReplCommand line of 
                ParseError err -> do 
                    printResponse $ (red colorOpts "Error") ++ ": " ++ show err
                    loop
                DoneParsing blicmd -> do
                    result <- lift $ handleBliReplCommand blicmd
                    case result of
                      True  -> loop
                      False -> return ()
                -- If the user has not entered a REPL command, try processing
                -- their input as a standard Bedelibry Prolog command.
                ContinueParsing -> do
                  -- Handle typo suggestions
                  if ( line /= "" && head line == ':')
                    then do
                      if line == ":" 
                        then do 
                          printResponse $ (red colorOpts "Error")++": \":\" must be followed by a valid command." 
                          loop
                        else do
                          let fuzzySet = fromList $ map Text.pack commandStringsAll :: FuzzySet
                          case getOne fuzzySet $ Text.pack (tail line) of
                            Just suggestion -> do 
                              printResponse $ (red colorOpts "Error")++
                                ": Command \""++tail line++"\" not found. Did you mean \""++
                                (tail $ Text.unpack suggestion)++"\"?"
                            Nothing -> do 
                              printResponse $ (red colorOpts "Error")++": Command \""++tail line++"\" not found."
                          loop
                    else do 
                      lift $ processCliInput line
                      loop

-- | Run the application as a thin client connecting to
--   a remote Bedelibry Prolog server.
thinClient :: Bli ()
thinClient = do 
  -- Handle connection to the server and exception handling... 

  -- Start the client
  runInputT defaultSettings loop
  where loop = do
          config    <- lift $ getConfig
          facts     <- lift $ getFacts
          types     <- lift $ getTypes
          relations <- lift $ getRelations
          entities  <- lift $ getEntities
          aliases   <- lift$ getAliases
          let colorOpts = not $ nocolor config
          -- liftIO $ setCompletionEntryFunction $ Just completionFunction
          maybeLine <- getInputLine (blue colorOpts command_prompt)
          case maybeLine of
            Nothing -> loop
            Just line -> do
              -- Add the user's input to the command line history.
              -- liftIO $ addHistory line
              case parseBliReplCommand line of 
                ParseError err -> do 
                    printResponse $ (red colorOpts "Error") ++ ": " ++ show err
                    loop
                DoneParsing blicmd -> do
                    result <- lift $ handleThinClientCommand blicmd
                    case result of
                      True  -> loop
                      False -> return ()
                -- If the user has not entered a REPL command, try processing
                -- their input as a standard Bedelibry Prolog command.
                ContinueParsing -> do
                  -- Handle typo suggestions
                  if ( line /= "" && head line == ':')
                   then do
                     if line == ":" 
                       then do 
                         printResponse $ (red colorOpts "Error")++": \":\" must be followed by a valid command." 
                         loop
                       else do
                         let fuzzySet = fromList $ map Text.pack commandStringsAll :: FuzzySet
                         case getOne fuzzySet $ Text.pack (tail line) of
                           Just suggestion -> do 
                             printResponse $ (red colorOpts "Error")++
                               ": Command \""++tail line++"\" not found. Did you mean \""++
                               (tail $ Text.unpack suggestion)++"\"?"
                           Nothing -> do 
                             printResponse $ (red colorOpts "Error")++": Command \""++tail line++"\" not found."
                             loop
                    else do 
                      lift $ processThinClientInput line
                      loop

handleThinClientCommand :: BliReplCommand -> Bli Bool
handleThinClientCommand command = do
  printResponse "Not implemented."
  return False
          
handleBliReplCommand :: BliReplCommand -> Bli Bool
handleBliReplCommand blicmd = do
  let repl = return True
  let exit = return False
  config    <- getConfig
  facts     <- getFacts
  types     <- getTypes
  relations <- getRelations
  entities  <- getEntities
  aliases   <- getAliases
  let colorOpts = not $ nocolor config
  case blicmd of
      Help -> do
        screen <- liftIO $ replHelpScreen colorOpts
        printResponse screen
        repl
      Exit -> do
        exit
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
        if  (entities  == empty
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
        ifEnabledThenElse Aliases
          (do if aliases == empty
              then do 
                printResponse $ yellow colorOpts "No aliases in store."
              else do 
                -- Note: To make this work, I probably need a "to list" function
                -- for aliases, so that I can get this to print properly.
                mapM_ printResponse $ map (\(id1, id2) -> "alias " ++ id1 ++ " " ++ id2 ++ ".")  $ toKVList aliases)
          (printResponse "Aliases have not been enabled.")
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
                    parseResult <- liftFromPure $ clausesFromString fileContents
                    case parseResult of
                        Left e -> printResponse "There has been a parse error."
                        Right clauses -> do
                             printResponse "Need to implement the logic for adding clauses here."
                             -- modifyProgram (\x -> x ++ clauses)
                  BliPlExtension  -> do
                     -- Currently this will only parse the typed version
                     parseResult <- liftFromPure $ parseTypedBli fileContents
                     case parseResult of
                         Left e -> printResponse "There has been a parse error."
                         Right program -> do
                             -- Note: We still need to do typechecking of the file here!
                             let lines = getCommands program
                             let (types, relations, entities, clauses) = groupSchemaClauses lines
                             newEntities entities
                             newFacts clauses
                             newTypes types
                             newRelations relations
                             return ()
                  SchemaFileExtension -> do
                     parseResult <- liftFromPure $ parseTypedSchema fileContents
                     case parseResult of
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
        parseResult <- liftFromPure $ parseBli atomP input
        case parseResult of
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
