
--
-- | The main parsers for bli prolog files
--

module Bli.Prolog.Parser where

import Data.BliParser
import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.Util
import Bli.Prolog.Parser.Terms
import Bli.Prolog.Parser.Schema
import Bli.Prolog.Parser.DateTime
import Bli.Prolog.Typechecking (collectGoalVars)
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Control.Monad (join)

-- | Loads a plain prolog file @filename@, and parses it into a list of clauses.
clausesFromFile :: String -> IO (Either ParseError [Clause])
clausesFromFile filename = parseFromFile prologProgramP filename

-- | Parses a plain prolog file directly from a string into a list of clauses.
clausesFromString :: String -> Either ParseError [Clause]
clausesFromString context = parse prologProgramP "" context

-- | Loads a bli file, and parses it.
parseTypedBliFile :: String -> IO (Either ParseError BliProgram)
parseTypedBliFile = parseFromFile bliPrologProgramP 

-- | Parses a bli file directly from its string representation
parseTypedBli :: String -> Either ParseError BliProgram
parseTypedBli = parse bliPrologProgramP ""

-- | Parser for a pure prolog program. 
prologProgramP :: Parser [(Term, Terms)]
prologProgramP = do spacesOrComments
                    clauses <- many1 clauseP
                    return $ clauses

-- | Parser for a bli prolog schema. Only contains assertions. No queries.
bliPrologSchemaP :: Parser BliProgram
bliPrologSchemaP = do
  lines' <- many $ try typedSchemaLineP `eitherP` clauseP
  let lines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> AssertClause clause) lines'
  return lines

-- | Parser for a bli prolog program.
bliPrologProgramP :: Parser BliProgram
bliPrologProgramP = do
  slines' <- many $ try typedSchemaLineP `eitherP` clauseP
  -- Symbol to indicate that we are done with definitions, and
  -- everything from now on is to be interpreted as a command.
  plines' <- option [] (do
    symb "?-"
    csymb '{'
    lines <- many goalP
    csymb '}'
    return lines)
  let slines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> AssertClause clause) slines'
  let plines = map Query $ map (\x -> (collectGoalVars x,x)) plines'
  
  return $ slines ++ plines

-- | Parser for the assertion of a prolog clause.
assertClauseP :: Parser Clause
assertClauseP = do t <- termP
                   body <- option []
                        (symb ":-" >> termsP)
                   csymb '!'
                   return (t, body)

-- | Bli Parser for the assertion of a prolog clause, using a Bli Parser 
--   (This is just for testing)
assertClauseBliP :: BliParser Clause
assertClauseBliP = 
  do t <- liftParser termP
     body <- option []
        (liftParser (symb ":-") >> liftParser termsP)
     liftParser $ csymb '!'
     return (t, body)

-- | Parser for a lambda query.
lambdaGoalP :: Parser LambdaGoal
lambdaGoalP = do skipMany (space >> return ())
                 csymb '\\' <|> csymb 'λ' <|> csymb 'Λ'
                 vars <- variableP `sepBy` (csymb ',')
                 csymb '.'
                 ts <- termsP
                 csymb '.'
                 return (vars, ts)                

-- | Parser for an assertion -- a prolog goal ending with a ! instead of a .
assertionP :: Parser Goal
assertionP = do ts <- termsP
                csymb '!'
                return ts


