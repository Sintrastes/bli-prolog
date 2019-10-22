
--
-- | The main parsers for the command line interface to bedelibry.
--

module Bli.Prolog.Parser.Cli where

import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Data.Bli.Prolog.Ast
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser

-- Note: In the future, we will probably have 
-- a seperate syntax for making schema declarations
-- in the cli, so this import don't be needed.
import Bli.Prolog.Parser.Schema

-- | Parse a typed bedelibry prolog command from a string.
parseBliCommandTyped string = parse bliCommandTypedP "" string

-- | Parser for a bedelibry command.
bliCommandTypedP :: Parser BliCommand
bliCommandTypedP = do 
  result <- (try (terminated typedSchemaLineP) `eitherP` (try (terminated assertClauseP) `eitherP` (try (terminated assertionP) `eitherP` (try (terminated goalP) `eitherP` (terminated lambdaGoalP)))))
  case result of
     Left x  -> return $ x
     Right x -> case x of 
         Left y  -> return $ AssertClause y
         Right y -> case y of 
            Left  z -> return $ AssertMode z
            Right z -> case z of
                Left w  -> return $ QueryMode w
                Right w -> return $ LambdaQuery w