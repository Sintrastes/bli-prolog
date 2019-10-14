
--
-- | The main parsers for bli prolog files
--

module Bli.Prolog.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.Schema
import Data.Bli.Prolog.Ast
import Data.Schema
import Control.Monad (join)

-- | Loads a plain prolog file @filename@, and parses it into a list of clauses.
clausesFromFile filename = parseFromFile programP filename

-- | Parses a plain prolog file directly from a string into a list of clauses.
clausesFromString context = parse programP "" context

-- | Loads a bli file, and parses it.
parseTypedBliFile = parseFromFile typedBliFileP 

-- | Parses a bli file directly from its stirng representation
parseTypedBli = parse typedBliFileP ""

-- | Parser for a pure prolog program. 
programP :: Parser Program
programP = do spacesOrComments
              clauses <- many1 clauseP
              return clauses

typedBliFileP :: Parser [BliCommandTyped]
typedBliFileP = do
  lines' <- many $ try typedSchemaLineP `eitherP` clauseP
  let lines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> T_AssertClause clause) lines'
  return lines
