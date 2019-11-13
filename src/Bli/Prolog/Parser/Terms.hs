
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
import Debug.Trace

-- | Parser for a plain prolog goal.                
goalP :: BliParser Goal
goalP = trace "In goal parser" $ 
        do ts <- termsP
           csymb '.'
           return ts  

-- | Parser for a term that uses equational syntax
equationalTermP :: BliParser Term
equationalTermP = trace "In equational term parser." $
                  do (Comp t args) <- termP'
                     csymb '='
                     y <- termP'
                     csymb '.'
                     return $ Comp t (args ++ [y])

-- | Parser for a plain prolog clause, parsed as a Rule. 
ruleP :: BliParser Term
ruleP = trace "In rule parser" $
        do csymb '{'
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
       -- <|> try appTermP
       <|> (Identifier <$> identifierP) 
  return res
  where
    quotedP = (do q <- char '"'
                  s <- manyTill anyChar (try $ char '"')
                  return $ StringLiteral s ) <?> "string literal"

-- | Parser for a bedelibry prolog atom which is not a higher-order term.
simpleAtomP :: BliParser Atom
simpleAtomP = do
  res <- try (AtomVar <$> variableP)
       <|> try quotedP
       <|> try floatLiteralP
       <|> try intLiteralP
       <|> try dataConstructorP
       <|> try dataConstructorEmptyP
       <|> try (TimeperiodLiteral <$> timePeriodP)
       <|> (Identifier <$> identifierP) 
  return res
  where
    quotedP = (do q <- char '"'
                  s <- manyTill anyChar (try $ char '"')
                  return $ StringLiteral s ) <?> "string literal"


-- | Parser for a prolog term which is not an equational term.
termP' :: BliParser Term
termP' =  do
  many space 
  try (variableP >>= return . Var)
     <|> try ruleP
     -- I don't think this is right -- the infix 
     -- term here is parsed as an atom, not an honest
     -- to goodness term.
     <|> try (ifEnabledP InfixOperators $ (\x -> Comp x []) <$> infixTermP)
     <|> try (literalP)
     <|> ((listP) <?> "list term")

-- | Parser for a prolog term.
termP :: BliParser Term
termP = do
  many space 
  try ruleP
     -- I don't think this is right -- the infix 
     -- term here is parsed as an atom, not an honest
     -- to goodness term.
     <|> try (ifEnabledP InfixOperators $ (\x -> Comp x []) <$> infixTermP)
     <|> try (literalP)
     <|> try (variableP >>= return . Var)
     <|> try (ifEnabledP EquationalSyntax $ equationalTermP)
     <|> ((listP) <?> "list term")


-- A top level term -- each of the alternatives must consume all of their
-- input to be valid.
topLevelTermP :: BliParser Term
topLevelTermP = do
  many space
  try (variableP >>= return . Var)
      <|> try (terminated ruleP)
      <|> try (ifEnabledP InfixOperators $ (\x -> Comp x []) <$> terminated infixTermP)
      <|> try (terminated literalP)
      <|> try (ifEnabledP EquationalSyntax $ terminated equationalTermP)
      <|> terminated ((listP) <?> "list term")

-- I guess this is syntax for a SNOC list?
listTermsP :: BliParser Term
listTermsP =
    do heads <- termsP
       tail  <- option emptyListTerm
                      (csymb '|' >> termP)
       return (foldr cons tail heads)

-- | Parser that handles lists as prolog terms.
listP :: BliParser Term
listP = trace "In list parser" $ do
  res <- between (csymb '[') (csymb ']')
                 (option emptyListTerm listTermsP)
  many space
  return res

-- | Parser for a list of prolog terms.
termsP :: BliParser Terms
termsP = sepBy1 (try ((\x -> Comp x []) <$> (ifEnabledP InfixOperators infixTermP)) <|> termP) (csymb ',')

-- Note: I don't think this is needed anymore.
-- | Parser for a list of prolog terms not containing any rules.
-- termsP' :: BliParser Terms
-- termsP' = sepBy1 termP' (csymb ',')

appTermP :: BliParser Atom
appTermP = do id <- identifierP
              char '('
              atoms <- atomP `sepBy` csymb ','
              char ')'
              return $ AppTerm id atoms 
        
{- -- Note: This should only be enabled
   -- for when we allow variable perdicates in the head
   -- of rules.              
varAppTermP :: BliParser Atom
varAppTermP = do id <- variableP
                 char '('
                 atoms <- atomP `sepBy` csymb ','
                 char ')'
                 return $ AppTerm (Var id) atoms 
-}

higherOrderTermP :: BliParser Term
higherOrderTermP = do 
  id <- appTermP
  terms <- parens termsP
  many space
  return $ Comp id terms

simpleTermP :: BliParser Term
simpleTermP = do
  id <- atomP
  terms <- parens termsP
  many space
  return $ Comp id terms

varTermP :: BliParser Term
varTermP = do
  id <- variableP
  terms <- parens termsP
  many space
  return $ Comp (AtomVar id) terms

atomicTermP :: BliParser Term
atomicTermP = do
       -- atomicTermP
       -- Note: I think this should really *not*
       -- be parsing predicates. So maybe a solution is
       -- to have a (seperate?) parser that does not parse
       -- predicate atoms.
       -- Note: disabling this entirely does not seem to work.
       -- Note: Also, changing this to simpleAtomP
       -- without changing the other instances of atomP
       -- to simpleAtomP
       (\x -> Comp x []) <$> atomP


-- | Parser for a prolog literal. Note: I don't think this is
--   very meaningful, we should try to refactor this out.
literalP :: BliParser Term
literalP = trace "In literal parser" $
           -- Each one of these can be a seperate parser.
           try higherOrderTermP
       -- simpleTermP
       <|> try varTermP
       <|> try simpleTermP
       <|> atomicTermP

-- Note: I don't think this is needed anymore.
-- | Parser for a prolog literal which is not an atom.
-- literalP' :: BliParser Term
-- literalP' = 
--            try ( do id <- appTermP
--                     terms <- parens termsP
--                     return $ Comp id terms )
--        <|> try (do id <- atomP
--                    terms <- parens termsP
--                    return $ Comp id terms )
--        <|> (\x -> Comp x []) <$> atomP


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
