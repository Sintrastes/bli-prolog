
module Bli.Prolog.Parser.Terms where

import Data.Bli.Prolog.Ast
import Text.ParserCombinators.Parsec
import Bli.Prolog.Parser.Util
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.DateTime
import Bli.Prolog.Parser.Datatypes
import Bli.Prolog.Parser.Infix

-- | Parser for a plain prolog goal.                
goalP :: Parser Goal
goalP = do ts <- termsP
           csymb '.'
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

-- | Parser for a bedelibry prolog identifier.
atomP :: Parser Atom
atomP = do
  res <- (Identifier <$> identifierP) 
       <|> try (AtomVar <$> variableP)
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
--    <|> ( (\x -> Comp x []) <$> infixTermP)
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

-- | Parser that handles lists as prolog terms.
listP :: Parser Term
listP = do
  res <- between (csymb '[') (csymb ']')
                 (option emptyListTerm listTermsP)
  many space
  return res

-- | Parser for a list of prolog terms.
termsP :: Parser Terms
termsP = sepBy1 (try ((\x -> Comp x []) <$> infixTermP) <|> termP) (csymb ',')

-- | Parser for a list of prolog terms not containing any rules.
termsP' :: Parser Terms
termsP' = sepBy1 termP' (csymb ',')

appTermP :: Parser Atom
appTermP = do id <- identifierP
              char '('
              atoms <- atomP `sepBy` csymb ','
              char ')'
              return $ AppTerm id atoms  

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

-- Parse an application of a binary infix operator to two terms.
infixTermP :: Parser Atom
infixTermP = do
  atom1 <- (AtomVar <$> variableP) <|> (try atomP) <|> infixTermParensP
  many space
  op    <- operatorP
  many space
  atom2 <- (try infixTermP) <|> (AtomVar <$> variableP) <|> (try atomP) <|> infixTermParensP
  return $ AppTerm op [atom1, atom2] 

infixTermParensP :: Parser Atom
infixTermParensP = do
  csymb '('
  atom1 <- (AtomVar <$> variableP) <|> (try infixTermParensP) <|> atomP
  many space
  op    <- operatorP
  many space
  atom2 <- (AtomVar <$> variableP) <|> (try infixTermParensP) <|> atomP
  csymb ')'
  return $ AppTerm op [atom1 , atom2] 
