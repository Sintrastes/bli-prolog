
--
-- | The main parsers for bli prolog files
--

module Bli.Prolog.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.Schema
import Bli.Prolog.Parser.DateTime
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
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

-- | Parser for a prolog term.
termP :: Parser Term
termP =  (variableP >>= return . Var)
    <|> literalP
    <|> (listP     <?> "list term")

-- I guess this is syntax for a SNOC list?
listTermsP :: Parser Term
listTermsP =
    do heads <- termsP
       tail  <- option emptyListTerm
                      (csymb '|' >> termP)
       return (foldr cons tail heads)

-- | Parser for the assertion of a prolog clause.
assertClauseP :: Parser Clause
assertClauseP = do t <- termP
                   body <- option []
                        (symb ":-" >> termsP)
                   csymb '!'
                   return (t, body)

-- | Parser that handles lists as prolog terms.
listP :: Parser Term
listP = between (csymb '[') (csymb ']')
               (option emptyListTerm listTermsP)


-- | Parser for a lambda query.
lambdaGoalP :: Parser LambdaGoal
lambdaGoalP = do skipMany (space >> return ())
                 csymb '\\' <|> csymb 'λ' <|> csymb 'Λ'
                 vars <- variableP `sepBy` (csymb ',')
                 csymb '.'
                 ts <- termsP
                 csymb '.'
                 return (vars, ts)                

-- | Parser for a plain prolog goal.                
goalP :: Parser Goal
goalP = do ts <- termsP
           csymb '.'
           return ts

-- | Parser for an assertion -- a prolog goal ending with a ! instead of a .
assertionP :: Parser Goal
assertionP = do ts <- termsP
                csymb '!'
                return ts
-- | Parser for a plain prolog clause. Depreciated. 
clauseP :: Parser Clause
clauseP = do t <- termP
             body <- option []
                     (symb ":-" >> termsP)
             csymb '.'
             return (t, body)

-- | Parser for a prolog literal. (i.e. not a list term)
literalP :: Parser Term
literalP = do id <- atomP
              option (Comp id [])
                     (parens termsP >>= return . Comp id)
        -- I'm not sure if I'll even need this.
       {-  <|>
           do ts <- parens termsP
              return $ commas ts
                where commas [a] = a
                      commas (h:t) = Comp "," [h, commas t] -}

-- | Parser for a list of prolog terms.
termsP :: Parser Terms
termsP = sepBy1 termP (csymb ',')


-- | Parser for a bedelibry prolog identifier.
atomP :: Parser Atom
atomP = (Identifier <$> identifierP) 
     <|> symbolicP
     <|> quotedP
     <|> intLiteralP
     <|> TimeperiodLiteral <$> timePeriodP
  where
    -- I'm not sure how much of this is needed. I'll use an identifier here for now.
    symbolicP = Identifier <$> (many1 $ oneOf "#$&*+-./:<=>?@\\^~") <?> "symbolic atom"
    quotedP = (do q <- char '"'
                  s <- manyTill anyChar (try $ char '"')
                  return $ StringLiteral s ) <?> "string literal"

typedBliFileP :: Parser [BliCommandTyped]
typedBliFileP = do
  lines' <- many $ try typedSchemaLineP `eitherP` clauseP
  let lines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> T_AssertClause clause) lines'
  return lines
