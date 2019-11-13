
--
-- | The main parsers for the command line interface to bedelibry.
--

module Bli.Prolog.Parser.Cli where

import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Control.Monad.Combinators (eitherP)
import Control.Monad.Bli.Pure
import Bli.App.Api
import Data.Bli.Prolog.Ast
import Bli.Prolog.Typechecking (collectGoalVars)
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.Util
import Bli.Prolog.Parser
import Bli.Prolog.Parser.Terms
import Bli.Prolog.Parser.Common
import Data.BliParser

-- Note: In the future, we will probably have 
-- a seperate syntax for making schema declarations
-- in the cli, so this import don't be needed.
import Bli.Prolog.Parser.Schema

-- | Parse a typed bedelibry prolog command from a string.
parseBliCommandTyped :: String -> Bli (Either [BliResult] BliCommand)
parseBliCommandTyped string = parseBli bliCommandTypedP string

-- | Parser for a bedelibry command.
bliCommandTypedP :: BliParser BliCommand
bliCommandTypedP = do 
  result <- (try (terminated typedSchemaLineP) 
              `eitherP` (try (terminated assertClauseP) 
              `eitherP` (try (terminated assertionP) 
              `eitherP` (try (terminated goalP) 
              `eitherP` (terminated lambdaGoalP)))))
  case result of
     Left x  -> return $ x
     Right x -> case x of 
         Left y  -> return $ AssertClause y
         Right y -> case y of 
            Left  z -> return $ Assert z
            Right z -> case z of
                Left w  -> return $ Query (collectGoalVars w, w)
                Right w -> return $ Query w