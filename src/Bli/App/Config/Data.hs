
module Bli.App.Config.Data where

import System.Console.CmdArgs as CA hiding (program) -- default
import Data.Aeson (toJSON, parseJSON, FromJSON)
import qualified Data.Aeson as Aeson
import Data.Typeable
import GHC.Generics
import Bli.Prolog.SearchStrategies
import Bli.App.Config.Features
import Data.Data

-- | The default search method is breadth first search.
instance Default Search where
  def = BFS

-- | The command prompt to use for the application.
command_prompt = "?- "

-- | The string to prepend to all responses to terminal commands in the applicaion.
response_prompt = "  "

-- | The path (relative to the user's home directory) of 
--   the bedelibry data directory.
bedelibryDir = "/.bedelibry"

-- | The name of the file to use for the default configuration of
--   Bli Prolog. Relative to the directory of the user's
--   bedelibry data directory.
configFileName = "/config.yaml"

-- | The name of the file to use for the list of modules.
moduleFileName = "/modules.yaml"

-- | Pattern synonyms for our file extension types.
pattern BliPlExtension = ".bpl"
pattern PlainPlExtension = ".pl"
pattern SchemaFileExtension = ".bsc"

data ModuleData = ModuleData { name :: String, file_path :: String } deriving(Generic)

instance FromJSON ModuleData where

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
 | Cmd_GetPID deriving(Enum, Bounded)

-- | Status result to return from out BliReplCommand parser --
--   If this parser does not fail, then the Repl continues to parse
--   the string as a bli-prolog query or assertion.
data BliReplParseResult = 
    DoneParsing BliReplCommand
  | ParseError String
  | ContinueParsing


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
          , prompt'        :: Bool
          , burl'          :: String
          }
  deriving (Show, Data, Typeable)


-- | Datatype for the application configuration
data AppConfig = 
  AppConfig {
  -- Options configured at the command line 
    options :: Options,
  -- Version string
    version :: String,
  -- Options for the language
    languageOptions :: [LanguageOption]
  }
