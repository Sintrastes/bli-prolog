
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
clausesFromFile filename = parseFromFile prologProgramP filename

-- | Parses a plain prolog file directly from a string into a list of clauses.
clausesFromString context = parse prologProgramP "" context

-- | Loads a bli file, and parses it.
parseTypedBliFile = parseFromFile bliPrologProgramP 

-- | Parses a bli file directly from its stirng representation
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
  let plines = map QueryMode plines'
  
  return $ slines ++ plines

-- | Parser for a prolog term which is not a rule.
termP' :: Parser Term
termP' =  (variableP >>= return . Var)
    <|> try literalP
    <|> (listP     <?> "list term")

-- | Parser for a prolog term.
termP :: Parser Term
termP = do
   many space 
   (variableP >>= return . Var)
      <|> try ruleP
      <|> try (literalP)
      <|> ((listP) <?> "list term")


-- A top level term -- each of the alternatives must consume all of their
-- input to be valid.
topLevelTermP :: Parser Term
topLevelTermP = (variableP >>= return . Var)
    <|> try (terminated literalP)
    <|> try ((terminated listP) <?> "list term")
    <|> ruleP

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
listP = do
  res <- between (csymb '[') (csymb ']')
                 (option emptyListTerm listTermsP)
  many space
  return res

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

-- | Parser for a plain data constructor, like 'True.
dataConstructorEmptyP :: Parser Atom
dataConstructorEmptyP = do
  id <- constructorP 
  return $ DataLit id []

-- | Parser for a data constructor with arguments, like 'Url.
dataConstructorP :: Parser Atom
dataConstructorP = do
  id   <- constructorP
  args <- parens (atomP `sepBy1` csymb ',')
  return $ DataLit id args
  

-- | Parser for an assertion -- a prolog goal ending with a ! instead of a .
assertionP :: Parser Goal
assertionP = do ts <- termsP
                csymb '!'
                return ts
-- | Parser for a plain prolog clause, parsed as a Rule. 
ruleP :: Parser Term
ruleP = do csymb '{'
           t <- termP'
           symb ":-"
           body <- termsP
           csymb '}'
           return $ Comp (Rule t body) []

-- | Parser for a plain prolog clause, parsed as a Rule. 
clauseP :: Parser (Term, Terms)
clauseP = do t <- termP
             body <- option []
                     (symb ":-" >> termsP)
             csymb '.'
             return $ (t, body)


-- | Parser for a prolog literal. (i.e. not a list term)
literalP :: Parser Term
literalP = 
           try ( do id <- appTermP
                    terms <- parens termsP
                    many space
                    return $ Comp id terms )
       <|> try (do id <- atomP
                   terms <- parens termsP
                   many space
                   return $ Comp id terms )
       <|> (\x -> Comp x []) <$> atomP

-- | Parser for a prolog literal which is not an atom.
literalP' :: Parser Term
literalP' = 
           try ( do id <- appTermP
                    terms <- parens termsP
                    return $ Comp id terms )
       <|> try (do id <- atomP
                   terms <- parens termsP
                   return $ Comp id terms )
       <|> (\x -> Comp x []) <$> atomP

-- | Parser for a list of prolog terms.
termsP :: Parser Terms
termsP = sepBy1 termP (csymb ',')

-- | Parser for a list of prolog terms not containing any rules.
termsP' :: Parser Terms
termsP' = sepBy1 termP' (csymb ',')

-- Parse an application of a binary infix operator to two terms.
infixTermP :: Parser Atom
infixTermP = do
  atom1 <- atomP
  op    <- operatorP
  atom2 <- atomP
  return $ AppTerm op [atom1, atom2] 

appTermP :: Parser Atom
appTermP = do id <- identifierP
              char '('
              atoms <- atomP `sepBy` csymb ','
              char ')'
              return $ AppTerm id atoms  

-- | Parser for a bedelibry prolog identifier.
atomP :: Parser Atom
atomP = do
  res <- (Identifier <$> identifierP) 
    -- <|> try symbolicP
       <|> try quotedP
       <|> try floatLiteralP
       <|> try intLiteralP
       <|> try dataConstructorP
       <|> try dataConstructorEmptyP
       <|> TimeperiodLiteral <$> timePeriodP
  return res
  --justAtom <- try ((symb ":-") >> return False) <|> return True
  --case justAtom of
  --  True -> return res
  --  False -> fail "Not an atom."
  where
    -- I'm not sure how much of this is needed. I'll use an identifier here for now.
    -- symbolicP = Identifier <$> (many1 $ oneOf "#$&*+-./:<=>?@\\^~") <?> "symbolic atom"
    quotedP = (do q <- char '"'
                  s <- manyTill anyChar (try $ char '"')
                  return $ StringLiteral s ) <?> "string literal"
