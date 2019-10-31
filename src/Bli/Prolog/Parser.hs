
--
-- | The main parsers for bli prolog files
--

module Bli.Prolog.Parser where

import Data.BliParser
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
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
import Control.Monad.Bli.Pure
import Bli.App.Api
import Bli.App.Config.Features
import Bli.App.Config

-- Note: The lines below are depreciated, and need to be updated.

-- | Loads a plain prolog file @filename@, and parses it into a list of clauses.
-- clausesFromFile :: String -> IO (Either ParseError [Clause])
-- clausesFromFile filename = parseFromFile prologProgramP filename

-- | Parses a plain prolog file directly from a string into a list of clauses.
clausesFromString :: String -> Bli (Either [BliResult] [Clause])
clausesFromString context = parseBli prologProgramP context

parseBliPrologProgram :: String -> Bli (Either [BliResult] BliProgram)
parseBliPrologProgram = parseBli bliPrologProgramP 

-- | Parses a bli file directly from its string representation
-- parseTypedBli :: String -> Either ParseError BliProgram
parseTypedBli = parseBli bliPrologProgramP

-- | Parser for a pure prolog program. 
prologProgramP :: BliParser [(Term, Terms)]
prologProgramP = do spacesOrComments
                    clauses <- many1 clauseP
                    return $ clauses

-- | Parser for a bli prolog schema. Only contains assertions. No queries.
bliPrologSchemaP :: BliParser BliProgram
bliPrologSchemaP = do
  lines' <- many $ try typedSchemaLineP `eitherP` clauseP
  let lines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> AssertClause clause) lines'
  return lines

-- | Parser for a bli prolog program.
bliPrologProgramP :: BliParser BliProgram
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
assertClauseP :: BliParser Clause
assertClauseP = do t <- termP
                   body <- option []
                        (symb ":-" >> termsP)
                   csymb '!'
                   return (t, body)

-- | Parser for a lambda query.
lambdaGoalP :: BliParser LambdaGoal
lambdaGoalP = do skipMany (space >> return ())
                 csymb '\\' <|> ifEnabledP UnicodeSyntax (csymb 'λ' <|> csymb 'Λ')
                 vars <- variableP `sepBy` (csymb ',')
                 csymb '.'
                 ts <- termsP
                 csymb '.'
                 return (vars, ts)                

-- | Parser for an assertion -- a prolog goal ending with a ! instead of a .
assertionP :: BliParser Goal
assertionP = do ts <- termsP
                csymb '!'
                return ts


