{-# LANGUAGE DeriveGeneric #-}

module Bli.App.Config where

--
-- | Configuration  options for the bli-prolog executable.
--

import System.Console.CmdArgs as CA hiding (program)
import Bli.Prolog.SearchStrategies
import Bli.App.Config.Util
import Bli.App.Config.Version
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Yaml
import Control.Monad.IO.Class
import Data.Typeable
import Data.Scientific
import Control.Monad (join)
import Bli.App.Colors
import System.Directory
import System.Console.Terminal.Size
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import Data.Aeson (toJSON, parseJSON)
import qualified Data.Aeson as Aeson
import GHC.Generics

-- | The default search method is breadth first search.
instance Default Search where
  def = BFS

-- | The command prompt to use for the application.
command_prompt = "?- "

-- | The string to prepend to all responses to terminal commands in the applicaion.
response_prompt = "  "

-- | Helper function for printing a response prompt in the Bli monad.
printResponse string = liftIO $ putStrLn $ response_prompt ++ string

-- | The path (relative to the user's home directory) of 
--   the bedelibry data directory.
bedelibryDir = "/.bedelibry"

-- | The name of the file to use for the default configuration of
--   Bli Prolog. Relative to the directory of the user's
--   bedelibry data directory.
configFileName = "/config.yaml"

-- | The name of the file to use for the list of modules.
moduleFileName = "/modules.yaml"

data ModuleData = ModuleData { name :: String, file_path :: String } deriving(Generic)

instance FromJSON ModuleData where

-- Note: By default we can allow for cyclic imports,
-- and just keep a running list of the modules which have
-- already been imported, and stop when we try re-importing
-- modules which have already been imported.

-- | Helper function to get the Bli module data from
-- our CSV file.
getBliModuleData :: IO (Maybe [(String, String)])
getBliModuleData = do
  homeDir <- getHomeDirectory
  yaml <- decodeFileEither (homeDir ++ bedelibryDir ++ moduleFileName) :: IO (Either ParseException Object)
  case yaml of
    Left err -> return $ Nothing
    Right obj -> do
      case HashMap.lookup "modules" obj of
        Nothing -> return Nothing
        Just array -> do
          -- Make sure each of the elements of the array has the right
          -- structure.
          let maybeParsed = Aeson.decode $ Aeson.encode (toJSON array) :: Maybe [ModuleData]
          case maybeParsed of
            Just parsed -> return $ Just $ map (\(ModuleData x y) -> (x, y)) parsed
            Nothing -> return $ Nothing
-- | An abstract representation of the commands which
--   can be entered at the bli-prolog REPL.
data BliReplCommand =
   Help
 | Exit
 | ExportFile String
 | LoadFile String
 | Alias String String
 | ClearSchema
   | ClearRelations
   | ClearEntities
   | ClearFacts
 | ListSchema
   | ListRelations
   | ListTypes
   | ListEntities
   | ListFacts
 | ListAliases
 | SetMode String
 | GetTypeOf String
 | ShowPort
 | GetPID String

-- | An abstract representation of the different types of commands
--   which can be entered at the bli-prolog REPL.
data BliReplCommandType =
   Cmd_Help
 | Cmd_Exit
 | Cmd_ExportFile
 | Cmd_LoadFile
 | Cmd_Alias
 | Cmd_ClearSchema
 | Cmd_ClearRelations
 | Cmd_ClearEntities
 | Cmd_ClearFacts
 | Cmd_ListSchema
 | Cmd_ListRelations
 | Cmd_ListTypes
 | Cmd_ListEntities
 | Cmd_ListFacts
 | Cmd_ListAliases
 | Cmd_SetMode
 | Cmd_ShowPort
 | Cmd_GetTypeOf
 | Cmd_GetPID deriving(Enum)

-- | Takes a BliReplCommandType, and returns a list of the strings 
--   which can be used to invoke that command.
bliReplCommandStrings :: BliReplCommandType -> [String]
bliReplCommandStrings cmd = 
  case cmd of
    Cmd_Help           -> [":h",":help"]
    Cmd_Exit           -> [":exit",":quit",":q"]
    Cmd_ExportFile     -> [":export"]
    Cmd_LoadFile       -> [":load"]
    Cmd_Alias          -> [":alias"]
    Cmd_ClearFacts     -> [":clr-facts"] 
    Cmd_ClearSchema    -> [":clr-schema"]
    Cmd_ClearRelations -> [":clr-relations",":clr-rels"]
    Cmd_ClearEntities  -> [":clr-entities",":clr-ents"]
    Cmd_ListSchema     -> [":ls-schema"]
    Cmd_ListRelations  -> [":ls-relations", ":ls-rels"]
    Cmd_ListTypes      -> [":ls-types"]
    Cmd_ListEntities   -> [":ls-entities",":ls-ents"]
    Cmd_ListFacts      -> [":ls-facts"]
    Cmd_ListAliases    -> [":ls-aliases"]
    Cmd_SetMode        -> [":set-mode"]
    Cmd_GetTypeOf      -> [":t",":type"]
    Cmd_ShowPort       -> [":port"]
    Cmd_GetPID         -> [":pid"]
-- | Takes a BliReplCommandType, and returns just the primary string
--   which can be used to invoke that command.
bliReplCommandString :: BliReplCommandType -> String
bliReplCommandString cmd = head $ bliReplCommandStrings cmd

-- | Takes a BliReplCommandType, and returns a short description of
--   what that command does. 
bliReplCommandDescriptions :: BliReplCommandType -> String
bliReplCommandDescriptions cmd =
  case cmd of
    Cmd_Help           -> "Prints this help screen."
    Cmd_Exit           -> "Exits bli-prolog."
    Cmd_ExportFile     -> "Exports the definitions stored in bli-prolog's in-memory fact store to a file."
    Cmd_LoadFile       -> "Loads a schema or a prolog file into bli-prolog's in-memory sore."
    Cmd_Alias          -> "Attempts to add a new alias to the local alias store."
    Cmd_ClearRelations -> "Clears all relations (and facts) from the local store."
    Cmd_ClearEntities  -> "Clears all entities (and facts) from the local store."
    Cmd_ClearFacts     -> "Clears all facts from the local store."
    Cmd_ClearSchema    -> "Clears everything from the local schema."
    Cmd_ListSchema     -> "Lists the schema from the local store."
    Cmd_ListRelations  -> "Lists the relations from the local store."
    Cmd_ListEntities   -> "Lists the entities from the local store."
    Cmd_ListTypes      -> "Lists the types from the local store."
    Cmd_ListFacts      -> "Lists the facts from the local store."
    Cmd_ListAliases    -> "Lists the aliases from the local store."
    Cmd_SetMode        -> "Sets the search mode of the running session."
    Cmd_GetTypeOf      -> "Returns the type of the supplied bli prolog term."
    Cmd_ShowPort       -> "Shows the port number that the bli-prolog server is configured to run at."
    Cmd_GetPID         -> "Gets the primary identifier of the given term, if it exists."

typeToCommand :: BliReplCommandType -> Maybe BliReplCommand
typeToCommand ty = 
  case ty of
     Cmd_Help           -> Just Help
     Cmd_Exit           -> Just Exit
     Cmd_ExportFile     -> Nothing
     Cmd_LoadFile       -> Nothing
     Cmd_Alias          -> Nothing
     Cmd_SetMode        -> Nothing
     Cmd_GetTypeOf      -> Nothing
     Cmd_GetPID         -> Nothing
     Cmd_ClearSchema    -> Just ClearSchema
     Cmd_ClearRelations -> Just ClearRelations
     Cmd_ClearEntities  -> Just ClearEntities
     Cmd_ClearFacts     -> Just ClearFacts
     Cmd_ListSchema     -> Just ListSchema
     Cmd_ListRelations  -> Just ListRelations
     Cmd_ListTypes      -> Just ListTypes
     Cmd_ListEntities   -> Just ListEntities
     Cmd_ListFacts      -> Just ListFacts
     Cmd_ListAliases    -> Just ListAliases
     Cmd_ShowPort       -> Just ShowPort

-- | Status result to return from out BliReplCommand parser --
--   If this parser does not fail, then the Repl continues to parse
--   the string as a bli-prolog query or assertion.
data BliReplParseResult = 
    DoneParsing BliReplCommand
  | ParseError String
  | ContinueParsing

parseBliReplCommand :: String -> BliReplParseResult
parseBliReplCommand input =
  case lookup True $
     zip (map (\x -> input `elem` (bliReplCommandStrings x)) (enumValues @BliReplCommandType)) 
         (enumValues @BliReplCommandType) of
    Just cmd -> 
      case typeToCommand cmd of
        Just cmd -> DoneParsing $ cmd
        Nothing -> ParseError "Invalid format."
    Nothing -> 
      case () of
        -- TODO: Better error handling here.
        _ | any (\cmd -> isPrefixOf cmd input) (bliReplCommandStrings Cmd_LoadFile) ->
            DoneParsing $ LoadFile $ (splitOn " " input) !! 1
          | any (\cmd -> isPrefixOf cmd input) (bliReplCommandStrings Cmd_ExportFile) ->
            DoneParsing $ ExportFile $ (splitOn " " input) !! 1
          | any (\cmd -> isPrefixOf cmd input) (bliReplCommandStrings Cmd_Alias) ->
            DoneParsing $ Alias ((splitOn " " input) !! 1) ((splitOn " " input) !! 2)
          | any (\cmd -> isPrefixOf cmd input) (bliReplCommandStrings Cmd_SetMode) ->
            DoneParsing $ SetMode ((splitOn " " input) !! 1)
          | any (\cmd -> isPrefixOf cmd input) (bliReplCommandStrings Cmd_GetTypeOf) ->
            DoneParsing $ GetTypeOf ((splitOn " " input) !! 1)
          | any (\cmd -> isPrefixOf cmd input) (bliReplCommandStrings Cmd_GetPID) ->
            DoneParsing $ GetPID ((splitOn " " input) !! 1)
          | otherwise -> ContinueParsing

-- | The banner which is displayed when the user first loads the repl.
--   Note: To automate this, I need some sort of pretty printing library.
replBanner :: String -> Bool -> String
replBanner version colorOpts = foldr1 (\x -> \y -> x ++ "\n" ++ y) $
    [""
    ,"  |      |            |"
    ,"  |      |  .         |"
    ,"  |---|  |     |---|  |"
    ,"  |   |  |  |  |   |  |"
    ,"  |---|  |  |  |---|  |"
    ,"               |"
    ,"               |"
    ,"Welcome to the bli-prolog interpreter v" ++ version ++ "! (C) Nathan Bedell 2019"
    ,"Type "++(blue colorOpts $ bliReplCommandString Cmd_Help)++" for help, or "++(blue colorOpts $ bliReplCommandString Cmd_Exit)++" to quit."]

-- | Helper function to get all enum values
enumValues :: (Enum a) => [a]
enumValues = enumFrom (toEnum 0)

-- | Help screen to print when :h is called in the REPL
replHelpScreen :: Bool -> IO String
replHelpScreen colorOpts = do
  let footer = 
         ["Usage:"
         ,"  [PROLOG_TERM|PROLOG_CLAUSE]!   Assert a fact or rule."
         ,"  [PROLOG_TERM].                 Make a standard prolog query."
         ,"  \\[FREE_VARS]. [PROLOG_TERM].   Make a lambda query."
         ,""
         ,"For more information, please see the documentation at https://github.com/Sintrastes/bli-prolog."]
  -- Length of the largest command, used for formatting.
  let maxCommandLength = foldr1 max 
        $ map length
        $ map bliReplCommandString
            (enumValues @BliReplCommandType)
  -- The maximum width to use for our display, regardless of terminal size.
  let max_width = 98
  -- Number of spaces to put between columns
  let column_offset = 4
  -- Number of spaces to put at the beginning of the items in a list
  let initial_offset = 2
  -- Number of extra spaces to indent multi-line command descriptions
  let overhang = 2
  -- Optional prefix used when formatting commands.
  let description_prefix = "* "
  -- Additional offset needed to deal with color codes
  let magic_number = 10
  -- Helper function to append spaces to the end of a string until it reaches the desired length.
  let concatSpaces n str 
        | length str < n = concatSpaces n (str ++ " ")
        | otherwise = str
  let cmdColumn = map (\x -> (take initial_offset $ repeat ' ') ++ x) 
        $ map (concatSpaces (maxCommandLength + column_offset + magic_number))
        $ map (\str -> blue colorOpts str)
        $ map bliReplCommandString 
        $ enumValues @BliReplCommandType
  let descColumn = map bliReplCommandDescriptions (enumValues @BliReplCommandType)
  let cmdsAndDescs = zipWith (++) cmdColumn descColumn
  maybeWindow <- size
  case maybeWindow of
    Nothing -> 
      -- If we can't get the terminal size, then just use "standard" formatting.
      return $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
         ["Commands:"] ++
         cmdsAndDescs  ++ [""] ++
         footer
    Just (Window _ termWidth) -> do
      -- First split a string into words, and then reconstruct the string from the words, adding newlines and the appropriate number of spaces if
      -- adding the word would make a line larger than the width of the terminal
      let width = min termWidth max_width
      let removeLeadingWhitespace (' ':xs) = removeLeadingWhitespace xs
          removeLeadingWhitespace xs = xs
      let fmtHelp (wd:wrds) acc 
            | (length $ removeLeadingWhitespace $ last (splitOn "\n" (acc++" "++wd))) + column_offset + maxCommandLength + initial_offset >= width =
                 fmtHelp wrds (acc++"\n"++(take (column_offset + maxCommandLength + initial_offset + 1 + overhang) (repeat ' '))++wd)
            | otherwise = 
                 fmtHelp wrds (acc++" "++wd)
          fmtHelp [] acc = acc
      -- Helper function to format the descriptions in a readable way given the terminal width.
      let fmtDescr str = fmtHelp (words str) ""
      let cmdsAndFmtDescs = zipWith (++) cmdColumn (map fmtDescr $ map (description_prefix++) descColumn)
      return $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
         ["Commands:"] ++
         cmdsAndFmtDescs  ++ [""] ++
         footer

-- | A datatype for the possible options that can be configured by the user for the
--   bli-prolog executable. 
data Options =
  Options { search'        :: Search
          , program'       :: FilePath
          , schema'        :: FilePath
          , goal'          :: String
          , limit'         :: Maybe Int
          , depth'         :: Int
          , verbose'       :: Bool
          , nocolor'       :: Bool
          , json'          :: Bool
          , server'        :: Bool
          , bedelibryMode' :: String
          , port'          :: Maybe Int
          , burl'          :: String
          }
  deriving (Show, Data, Typeable)

instance IsRecord AppConfig where
  fromRecord record = do
    search <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "search" record :: Maybe (Maybe Search)) 
    program <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "program" record :: Maybe (Maybe FilePath)) 
    schema <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "schema" record :: Maybe (Maybe FilePath)) 
    goal <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "goal" record :: Maybe (Maybe String)) 
    limit <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "limit" record :: Maybe (Maybe (Maybe Int))) 
    depth <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "depth" record :: Maybe (Maybe Int)) 
    verbose <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "verbose" record :: Maybe (Maybe Bool)) 
    nocolor <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "nocolor" record :: Maybe (Maybe Bool)) 
    json <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "json" record :: Maybe (Maybe Bool)) 
    server <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "server" record :: Maybe (Maybe Bool)) 
    bedelibryMode <- join $ ((\(Typ x) -> cast x) <$>
                           Map.lookup "bedelibryMode" record :: Maybe (Maybe String)) 
    port <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "port" record :: Maybe (Maybe (Maybe Int))) 
    burl <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "burl" record :: Maybe (Maybe String))
    version <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "burl" record :: Maybe (Maybe String))
    return $ AppConfig (Options search program schema goal limit depth verbose
                                nocolor json server bedelibryMode port burl)
                        version
  toRecord (AppConfig options version) = 
      toRecord options #> 
      Map.fromList [("version", Typ version)]

instance IsRecord Options where
  fromRecord record = do
    search <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "search" record :: Maybe (Maybe Search)) 
    program <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "program" record :: Maybe (Maybe FilePath)) 
    schema <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "schema" record :: Maybe (Maybe FilePath)) 
    goal <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "goal" record :: Maybe (Maybe String)) 
    limit <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "limit" record :: Maybe (Maybe (Maybe Int))) 
    depth <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "depth" record :: Maybe (Maybe Int)) 
    verbose <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "verbose" record :: Maybe (Maybe Bool)) 
    nocolor <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "nocolor" record :: Maybe (Maybe Bool)) 
    json <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "json" record :: Maybe (Maybe Bool)) 
    server <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "server" record :: Maybe (Maybe Bool)) 
    bedelibryMode <- join $ ((\(Typ x) -> cast x) <$>
                           Map.lookup "bedelibryMode" record :: Maybe (Maybe String)) 
    port <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "port" record :: Maybe (Maybe (Maybe Int))) 
    burl <- join $ ((\(Typ x) -> cast x) <$> Map.lookup "burl" record :: Maybe (Maybe String))
    return $ Options search program schema goal limit depth verbose
                     nocolor json server bedelibryMode port burl
  toRecord (Options search program schema goal limit depth verbose
                    nocolor json server bedelibryMode port burl) =
     Map.fromList [("search", Typ search), ("program", Typ program), ("schema", Typ schema)
                  ,("goal", Typ goal), ("limit", Typ limit), ("depth", Typ depth)
                  ,("verbose", Typ verbose), ("nocolor", Typ nocolor), ("json", Typ json)
                  ,("server", Typ server), ("bedelibryMode", Typ bedelibryMode)
                  ,("port", Typ port), ("burl", Typ burl)]


-- | Datatype for the application configuration
data AppConfig = 
  AppConfig {
  -- Options configured at the command line 
    options :: Options,
  -- Other options
    version :: String
  }

search (AppConfig options _) = search' options
program (AppConfig options _) = program' options
schema (AppConfig options _) = schema' options
goal (AppConfig options _) = goal' options
limit (AppConfig options _) = limit' options
depth (AppConfig options _) = depth' options
verbose (AppConfig options _) = verbose' options
nocolor (AppConfig options _) = nocolor' options
json (AppConfig options _) = json' options
server (AppConfig options _) = server' options
port (AppConfig options _) = port' options
burl (AppConfig options _) = burl' options
 
-- | Note: These are things which should not be able to be configured through
--   the command line. Or, if they are, it should be through a very specific interface
--   (like Haskell's -XWhateverExtension).
--
--   Most likely, these should be read from a configuration file, or from
--   the command line, or using language pragmas. 
--
--   This is mostly for me to experiment around with different things. I will
--   probably set some defaults eventually
data LanguageOption =

 -- | This gives access to the untyped version of bli prolog, and the original syntax
 -- for schema files, if anyone wants access to that fragment of the language.
 -- Note: Keeping this around might be interesting in the future, e.x. for
 -- implementing some form of gradual typing with this syntax.
    UntypedLanguage
 -- | When set, this allows for
 --   names to be used unambigously in different ways.
 --
 --   For instance, programming_langauge can refer to a type, as well as
 --   a unary predicate on entities.
 --
 --   However, this extension allows user-declared predicates to have multiple
 --   roles. For instance, a predicate "pred : type -> type -> type" under this
 --   extension is considered to be different from a predicate "pred: type -> type".   
  |  PredicateOverloading
 -- | This works similarly to the above, except it allows for entities
 --   with the same name to be declared of different types.
 --   This is a bit more confusing, because of the ambiguity in how this
 --   should be interpreted semantically. Does 
 --
 --       nate : person
 --       nate : animal
 --
 --   mean that nate (person) and nate (animal) refer to two different entities,
 --   or does this mean that nate is both a person and an animal?
 --
 --   This makes entity overloading a more difficult extension to implement.
 --   Perhaps this might be better a multiple extensions with slightly different
 --   behavior. Otherwise, we need a system here for how these sort of ambiguities
 --   will be handled.   
  | EntityOverloading
 --   This extension is pretty straightforward (I think). It allows for an identifier
 --   to stand for both an entity, and for a predicate. For instance, programming_language
 --   is an entity of type *type*, whereas programming_language is also a unary predicate.
 --   With this extension set, users are allowed to declare their own identifiers
 --   as being predicates and as being entities. This should be relatively straightforward
 --   because there is no way of confusing the semantics here.
  | PredicateEntityOverloading

-- | Starting options for the bli-prolog exectuable.
startOptions version =
  Options { search' = def &= help "Specify wether to use DFS, BFS, or Limited"
          , program' = def &= typFile &= help "Prolog file with clauses"
          , schema' = def &= typFile &= help "Schema file"
          , limit' = def &= help "Limit the number of solutions found"
          , depth' = 100 &= help "Maximum depth to traverse when using limited search"
          , goal' = def &= args &= typ "GOALSTRING"
          , nocolor' = False &= help "Turn off colors in the REPL."
          , verbose' = True &= help "Specify whether or not to use verbose output (on by default)"
          , json' = False &= help "Specify whether or not json output formatting is used for queries."
          , server' = False &= help "Starts a REST server for processing bli prolog queries if set."
          , port' = def &= help "Port number to start the server."
          , burl' = def &= help "URL of the bedelibry server configured to work with bli-prolog."
          -- For example, bli-prolog can be configured to only send assertions to the server
          -- on an explicit :export-bedelibry command, or this can be done automatically.
          -- In addition, we have this option both for entities, and for facts.
          , bedelibryMode' = "" &= help "Sets the mode of interaction between bli-prolog and the bedebliry server."
          }
  &= summary ("bli-prolog interpreter v" ++ version ++ ", (C) Nathan Bedell 2019")

-- | Loads the version information from the .cabal file,
--   loads the configuration data from both the
--   configuration file, and from the command line arguments,
--   and then combines these options to return the AppConfig,
--   letting the command line options override the defaults
--   in the config file.
configureApplication :: IO (Either String AppConfig)
configureApplication = do
  -- Get the version from the cabal file at compile-time.
  homeDir <- getHomeDirectory
  let v = $(getVersionFromCabal)
  case v of 
    Nothing -> return $ Left "Error getting version from cabal file."
    Just version -> do
      opts <- toRecord <$> (cmdArgs $ startOptions version)
      -- Todo: Do better error handling here.
      config <- decodeFileEither $ 
         homeDir ++ bedelibryDir ++ configFileName :: IO (Either ParseException Object)
      -- Parse yaml
      case config of
        Left err -> do
          error $ show err
        Right object -> do
          -- Get all the fields from the hashmap
          let searchF = HashMap.lookup "search" object 
          let programF = HashMap.lookup "program" object 
          let schemaF = HashMap.lookup "schema" object 
          let limitF = HashMap.lookup "limit" object 
          let depthF = HashMap.lookup "depth" object 
          let goalF = HashMap.lookup "goal" object 
          let nocolorF = HashMap.lookup "nocolor" object 
          let jsonF = HashMap.lookup "json" object 
          let serverF = HashMap.lookup "server" object 
          let portF = HashMap.lookup "port" object 
          let burlF = HashMap.lookup "burl" object 
          let bedelibryModeF = HashMap.lookup "bedelibryMode" object 
          let verboseF = HashMap.lookup "verbose" object
          -- Now, I'll want to do something here in the Maybe monad to send
          -- each one of these to a Typ field.
          
          -- Note: This code is confusing, I probably don't need to use a maybe monad here.
          -- Also, this logic should probably be in a different function/module.
          let maybeFields = do 
               -- search <- searchF
               let searchF' =
                     case searchF of
                       -- Note: Here we need to do some parsing to get the right type
                       Just (String str) -> Just $ Typ str
                       _          -> Nothing     
  
               -- program <- programF
               let programF' =
                     case programF of
                       Just (String str) -> Just $ Typ str
                       _          -> Nothing
  
               -- schema <- schemaF
               let schemaF' =
                     case schemaF of
                       Just (String s) -> Just $ Typ s
                       _        -> Nothing

               -- goal <- goalF
               let goalF' =
                     case goalF of
                       Just (String s) -> Just $ Typ s
                       _ -> Nothing

               let limitF' =
                     case limitF of
                       Nothing -> Just $ Typ (Nothing :: Maybe Int)
                       -- Note: I'll need to do additional parsing here to deal with this.
                       Just (Number n) -> Just $ Typ $ Just n
                       _ -> Nothing
  
               --- depth <- depthF
               let depthF' =
                     case depthF of
                        -- Note: I'll need to do additional parsing here.
                        Just (Number n) -> Just $ Typ n
                        _ -> Nothing
             
               -- verbose <- verboseF
               let verboseF' =
                     case verboseF of
                       Just (Bool b) -> Just $ Typ b
                       _ -> Nothing
             
               -- nocolor <- nocolorF
               let nocolorF' =
                     case nocolorF of
                       Just (Bool b) -> Just $ Typ b
                       _ -> Nothing
             
               -- json <- jsonF
               let jsonF' =
                     case jsonF of
                       Just (Bool b) -> Just $ Typ b
                       _ -> Nothing
             
               -- server <- serverF
               let serverF' = 
                     case serverF of
                       Just (Bool b) -> Just $ Typ b
                       _ -> Nothing
             
               -- bedelibryMode <- bedelibryModeF
               let bedelibryModeF' =
                     case bedelibryModeF of
                       Just (String s) -> Just $ Typ s
                       _ -> Nothing
             
               let portF' =
                     case portF of
                       -- Note: I need to do additional parsing here.
                       Just (Number n') -> 
                         case floatingOrInteger n' of
                           Right n -> Just $ Typ $ Just (n :: Int)
                           Left _  -> error "Bad port number format in config file."
                       Nothing -> Just $ Typ $ (Nothing :: Maybe Int)
                       _ -> Nothing
             
               -- burl <- burlF
               let burlF' =
                     case burlF of
                       Just (String s) -> Just $ Typ $ s
                       _ -> Nothing
                  
               let unwrapMaybe (x, Just y) = Just (x,y)
                   unwrapMaybe (x, Nothing) = Nothing
               -- Finally, collect all of the fields into a record
               return $ map unwrapMaybe [("search", searchF'), ("program", programF'),
                         ("schema",schemaF'), ("goal",goalF'),
                         ("limit",limitF'), ("depth", depthF'),
                         ("verbose",verboseF'),("nocolor",nocolorF'),
                         ("json",jsonF'),("server",serverF'),
                         ("bedelibryMode",bedelibryModeF'),
                         ("port",portF'),("burl",burlF')]
               -- Extracts all the "Just" entries, removing the "Nothing"s
          let fromJust (Just x) = x
          let newRecordFields = mapMaybe id $ fromJust $ maybeFields
          let yamlRecord = Map.fromList newRecordFields
  
          -- Return the final user configuration.
          case fromRecord $ yamlRecord #> opts of
             Just record ->
               return $ Right record
             Nothing -> return $ Left "Error parsing configuration."
          --return $ Right $ AppConfig { version = version, options = fromRecord $ opts }