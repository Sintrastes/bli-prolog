
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

-- | The banner which is displayed when the user first loads the repl.
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
    ,"Type "++(blue colorOpts ":h")++" for help, or "++(blue colorOpts ":exit")++" to quit."]

-- | Help scree to print when :h is called in the REPL
replHelpScreen :: Bool -> String
replHelpScreen colorOpts = foldr1 (\x -> \y -> x ++ "\n" ++ y) $
  ["Commands: "
  ,"  "++(blue colorOpts ":h")++"       Prints this help screen."
  ,"  "++(blue colorOpts ":exit")++"    Exits bli-prolog."
  ,"  "++(blue colorOpts ":export")++"  Exports the definitions stored in bli-prolog's in-memory fact store as assertions"
  ,"           to a file."
  ,"  "++(blue colorOpts ":load")++"    Loads a schema or a prolog file into bli-prolog's in-memory store."
  ,""
  ,"Usage:"
  ,"  [PROLOG_TERM|PROLOG_CLAUSE]!   Assert a fact or rule."
  ,"  [PROLOG_TERM].                 Make a standard prolog query."
  ,"  \\[FREE_VARS]. [PROLOG_TERM].   Make a lambda query."
  ,""
  ,"For more information, please see the documentation at https://github.com/Sintrastes/bli-prolog."]

-- | A datatype for the possible options that can be configured by the user for the
--   bli-prolog executable. 
data Options =
  Options { search'    :: Search
          , program'   :: FilePath
          , schema'    :: FilePath
          , goal'      :: String
          , limit'     :: Maybe Int
          , depth'     :: Int
          , verbose'   :: Bool
          , nocolor'   :: Bool
          , json'      :: Bool
          , server'    :: Bool
          , port'      :: Maybe Int
          , burl'      :: String
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