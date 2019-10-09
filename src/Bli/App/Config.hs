
module Bli.App.Config where

--
-- | Configuration  options for the bli-prolog executable.
--

import System.Console.CmdArgs as CA hiding (program)
import Prolog.Interp
import Bli.App.Config.Version
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List.Split
import Data.List
import Control.Monad (join)
import Bli.App.Colors
import System.Directory
import System.Console.Terminal.Size

-- | The command prompt to use for the application.
command_prompt = "?- "

-- | The string to prepend to all responses to terminal commands in the applicaion.
response_prompt = "  "

-- | An ADT representing all of the different search 
--   algorithms bli prolog can be configured to run with.
data Search = DFS | BFS | Limited
            deriving (Show, Eq, Data, Typeable)

-- | The default search method is breadth first search.
instance Default Search where
  def = BFS

-- | Helper function for converting our Search ADT into
--   actual search functions.
searchFunction DFS _     = dfs
searchFunction BFS _     = bfs
searchFunction Limited n = limitedDfs n

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
 | Cmd_SetMode deriving(Enum)

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

typeToCommand :: BliReplCommandType -> Maybe BliReplCommand
typeToCommand ty = 
  case ty of
     Cmd_Help           -> Just Help
     Cmd_Exit           -> Just Exit
     Cmd_ExportFile     -> Nothing
     Cmd_LoadFile       -> Nothing
     Cmd_Alias          -> Nothing
     Cmd_SetMode        -> Nothing
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
          , schema' = "" &= typFile &= help "Schema file"
          , limit' = def &= help "Limit the number of solutions found"
          , depth' = 100 &= help "Maximum depth to traverse when using limited search"
          , goal' = def &= args &= typ "GOALSTRING"
          , nocolor' = False &= help "Turn off colors in the REPL."
          , verbose' = True &= help "Specify whether or not to use verbose output (on by default)"
          , json' = False &= help "Specify whether or not json output formatting is used for queries."
          , server' = False &= help "Starts a REST server for processing bli prolog queries if set."
          , port' = Nothing &= help "Port number to start the server."
          , burl' = "" &= help "URL of the bedelibry server configured to work with bli-prolog."
          -- For example, bli-prolog can be configured to only send assertions to the server
          -- on an explicit :export-bedelibry command, or this can be done automatically.
          -- In addition, we have this option both for entities, and for facts.
          , bedelibryMode' = "" &= help "Sets the mode of interaction between bli-prolog and the bedebliry server."
          }
  &= summary ("bli-prolog interpreter v" ++ version ++ ", (C) Nathan Bedell 2019")

configureApplication :: IO (Either String AppConfig)
configureApplication = do
  -- Get the version from the cabal file at compile-time.
  homeDir <- getHomeDirectory
  let v = $(getVersionFromCabal)
  case v of 
    Nothing -> return $ Left "Error getting version from cabal file."
    Just version -> do
      opts <- cmdArgs $ startOptions version
      -- Todo: Do better error handling here.
      config <- readFile $ homeDir ++ "/.bedelibry/config.yaml"
      -- Parse yaml
      return $ Right $ AppConfig { version = version, options = opts }