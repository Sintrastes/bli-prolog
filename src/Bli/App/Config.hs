
module Bli.App.Config where

--
-- | Configuration  options for the bli-prolog executable.
--

import System.Console.CmdArgs as CA hiding (program)
import Prolog.Interp

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

-- | Help scree to print when :h is called in the REPL
replHelpScreen = foldr1 (\x -> \y -> x ++ "\n" ++ y) $
  ["Commands: "
  ,"  \27[36m:h\27[37m      Prints this help screen."
  ,"  \27[36m:exit\27[37m    Exits bli-prolog."
  ,"  \27[36m:export\27[37m  Exports the definitions stored in bli-prolog's in-memory fact store as assertions"
  ,"           to a file."
  ,"  \27[36m:load\27[37m    Loads a schema or a prolog file into bli-prolog's in-memory store."
  ,""
  ,"Usage:"
  ,"  [PROLOG_TERM|PROLOG_CLAUSE]!   Assert a fact or rule."
  ,"  [PROLOG_TERM].                 Make a standard prolog query."
  ,"  \\[FREE_VARS]. [PROLOG_TERM].   Make a lambda query."
  ,""
  ,"For more information, please see the documentation at https://github.com/Sintrastes/bli-prolog."]

-- | A datatype for the possible options that can be configured for the
--   bli-prolog executable. 
data Options =
  Options { search    :: Search
          , program   :: FilePath
          , schema    :: FilePath
          , goal      :: String
          , limit     :: Maybe Int
          , depth     :: Int
          , verbose   :: Bool
          , json      :: Bool
          , server    :: Bool
          , port      :: Maybe Int
          }
  deriving (Show, Data, Typeable)

-- | Starting options for the bli-prolog exectuable.
startOptions =
  Options { search = def &= help "Specify wether to use DFS, BFS, or Limited"
          , program = def &= typFile &= help "Prolog file with clauses"
          , schema = "" &= typFile &= help "Schema file"
          , limit = def &= help "Limit the number of solutions found"
          , depth = 100 &= help "Maximum depth to traverse when using limited search"
          , goal = def &= args &= typ "GOALSTRING"
          , verbose = True &= help "Specify whether or not to use verbose output (on by default)"
          , json = False &= help "Specify whether or not json output formatting is used for queries."
          , server = False &= help "Starts a REST server for processing bli prolog queries if set."
          , port = Nothing &= help "Port number to start the server."
          }
  &= summary "bli-prolog interpreter v0.3, (C) Nathan Bedell 2019"
