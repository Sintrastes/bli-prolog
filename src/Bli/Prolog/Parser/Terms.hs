
module Bli.Prolog.Parser.Terms where

import Data.Bli.Prolog.Ast
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Bli.Prolog.Parser.Util
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.DateTime
import Bli.Prolog.Parser.Datatypes
import Bli.Prolog.Parser.Infix
import Data.BliParser
import Bli.App.Config
import Bli.App.Config.Features

-- | Parser for a plain prolog goal.                
goalP :: BliParser Goal
goalP = do ts <- termsP
           csymb '.'
           return ts  

-- | Parser for a term that uses equational syntax
equationalTermP :: BliParser Term
equationalTermP = do (Comp t args) <- termP
                     csymb '='
                     y <- termP
                     csymb '.'
                     return $ Comp t (args ++ [y])

-- | Parser for a plain prolog clause, parsed as a Rule. 
ruleP :: BliParser Term
ruleP = do csymb '{'
           t <- termP'
           symb ":-"
           body <- termsP
           csymb '}'
           return $ Comp (Rule t body) []

-- | Parser for a plain prolog clause, parsed as a Rule. 
clauseP :: BliParser (Term, Terms)
clauseP = do t <- termP
             body <- option []
                     (symb ":-" >> termsP)
             csymb '.'
             return $ (t, body)

-- | Parser for a bedelibry prolog atom.
atomP :: BliParser Atom
atomP = do
  res <- try (AtomVar <$> variableP)
       <|> try quotedP
       <|> try floatLiteralP
       <|> try intLiteralP
       <|> try dataConstructorP
       <|> try dataConstructorEmptyP
       <|> try (TimeperiodLiteral <$> timePeriodP)
       <|> try appTermP
       <|> (Identifier <$> identifierP) 
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
termP' :: BliParser Term
termP' =  try (variableP >>= return . Var)
    <|> try literalP
    <|> (listP     <?> "list term")

-- | Parser for a prolog term.
termP :: BliParser Term
termP = do
   many space 
   try (variableP >>= return . Var)
      <|> try (ifEnabledP EquationalSyntax $ equationalTermP)
      <|> try ruleP
      <|> try ( (\x -> Comp x []) <$> infixTermP)
      <|> try (literalP)
      <|> ((listP) <?> "list term")


-- A top level term -- each of the alternatives must consume all of their
-- input to be valid.
topLevelTermP :: BliParser Term
topLevelTermP = try (variableP >>= return . Var)
    <|> try (terminated literalP)
    <|> try ((terminated listP) <?> "list term")
    <|> ruleP

-- I guess this is syntax for a SNOC list?
listTermsP :: BliParser Term
listTermsP =
    do heads <- termsP
       tail  <- option emptyListTerm
                      (csymb '|' >> termP)
       return (foldr cons tail heads)

-- | Parser that handles lists as prolog terms.
listP :: BliParser Term
listP = do
  res <- between (csymb '[') (csymb ']')
                 (option emptyListTerm listTermsP)
  many space
  return res

-- | Parser for a list of prolog terms.
termsP :: BliParser Terms
termsP = sepBy1 (try ((\x -> Comp x []) <$> infixTermP) <|> termP) (csymb ',')

-- | Parser for a list of prolog terms not containing any rules.
termsP' :: BliParser Terms
termsP' = sepBy1 termP' (csymb ',')

appTermP :: BliParser Atom
appTermP = do id <- identifierP
              char '('
              atoms <- atomP `sepBy` csymb ','
              char ')'
              return $ AppTerm id atoms  

-- | Parser for a prolog literal. (i.e. not a list term)
literalP :: BliParser Term
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
literalP' :: BliParser Term
literalP' = 
           try ( do id <- appTermP
                    terms <- parens termsP
                    return $ Comp id terms )
       <|> try (do id <- atomP
                   terms <- parens termsP
                   return $ Comp id terms )
       <|> (\x -> Comp x []) <$> atomP


-- | Parser for a plain data constructor, like 'True.
dataConstructorEmptyP :: BliParser Atom
dataConstructorEmptyP = do
  id <- constructorP 
  return $ DataLit id []

-- | Parser for a data constructor with arguments, like 'Url.
dataConstructorP :: BliParser Atom
dataConstructorP = do
  id   <- constructorP
  args <- parens (atomP `sepBy1` csymb ',')
  return $ DataLit id args

-- Parse an application of a binary infix operator to two terms.
infixTermP :: BliParser Atom
infixTermP = do
  atom1 <- (AtomVar <$> variableP) <|> (try atomP) <|> infixTermParensP
  many space
  op    <- operatorP
  many space
  atom2 <- (try infixTermP) <|> (AtomVar <$> variableP) <|> (try atomP) <|> infixTermParensP
  return $ AppTerm op [atom1, atom2] 

infixTermParensP :: BliParser Atom
infixTermParensP = do
  csymb '('
  atom1 <- (AtomVar <$> variableP) <|> (try infixTermParensP) <|> atomP
  many space
  op    <- operatorP
  many space
  atom2 <- (AtomVar <$> variableP) <|> (try infixTermParensP) <|> atomP
  csymb ')'
  return $ AppTerm op [atom1 , atom2] 
