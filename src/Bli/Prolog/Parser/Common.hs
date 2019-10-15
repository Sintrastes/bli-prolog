
--
-- | Parsers for generic prolog components (like atoms) which are used
--   by other more specifc parsers.

module Bli.Prolog.Parser.Common where

import Text.ParserCombinators.Parsec
import Data.Bli.Prolog.Ast

-- | Helper function to generate parsers which
--   are terminated by eof.
terminated parser = do
  result <- parser
  eof
  return result

-- | Parser for comments in a prolog file
commentP = singleLineComment <|> multiLineComment
  where
  singleLineComment = do char '%'
                         manyTill anyChar (try newline)
                         return ()

  multiLineComment = do try (string "/*")
                        inCommentMulti
    where
      inCommentMulti = (try (string "*/")            >> return ())
                   <|> (multiLineComment             >> inCommentMulti)
                   <|> (skipMany1 (noneOf startEnd)  >> inCommentMulti)
                   <|> (oneOf startEnd               >> inCommentMulti)
                   <?> "end of multi-line comment maker \"*/\""
      startEnd   = "/*/"


spacesOrComments = skipMany ((space >> return()) <|> commentP)

-- | Helper function for a symbol consisting of a single character
--   which can be surrounded by whitespace.
csymb c = (try(spacesOrComments >> char c) >> spacesOrComments)

-- | Helper function for a multi-character symbol which can be
--   surrounded by whitespace.
symb s = (try(spacesOrComments >> string s) >> spacesOrComments)

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

-- | Parser for the assertion of a prolog clause.
assertClauseP :: Parser Clause
assertClauseP = do t <- termP
                   body <- option []
                        (symb ":-" >> termsP)
                   csymb '!'
                   return (t, body)

-- | Parser for a prolog term.
termP :: Parser Term
termP =  (variableP >>= return . Var)
    <|> literalP
    <|> (listP     <?> "list term")


-- | Parser that handles lists as prolog terms.
listP :: Parser Term
listP = between (csymb '[') (csymb ']')
               (option emptyListTerm listTermsP)

-- I guess this is syntax for a SNOC list?
listTermsP :: Parser Term
listTermsP =
    do heads <- termsP
       tail  <- option emptyListTerm
                      (csymb '|' >> termP)
       return (foldr cons tail heads)

emptyListTerm :: Term
emptyListTerm = Comp (ListLiteral []) []

-- Todo: Make this explicitly a partial function and do error handling.
cons :: Term -> Term -> Term
cons (Comp h []) (Comp (ListLiteral ts) []) = Comp (ListLiteral (h:ts)) [] 

-- | Parser for a list of prolog terms.
termsP :: Parser Terms
termsP = sepBy1 termP (csymb ',')

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

parens :: Parser p -> Parser p
parens p = between (csymb '(') (csymb ')') p

identifierP =
        (do c <- lower
            cs <- many (alphaNum <|> char '_')
            case (c:cs) of
              "rel"  -> fail "\"rel\" is a reserved keyword, and may not be used as an identifer."
              "type" -> fail "\"type\" is a reserved keyword, and may not be used as an identifier."
              "entity" -> fail "\"entity\" is a reserved keyword, and may not be used as an identifer."
              otherwise -> return $ (c:cs)) <?> "atom"

-- | Parser for a bedelibry prolog identifier.
atomP :: Parser Atom
atomP = (Identifier <$> identifierP) <|> symbolicP <|> quotedP
  where
    -- I'm not sure how much of this is needed. I'll use an identifier here for now.
    symbolicP = Identifier <$> (many1 $ oneOf "#$&*+-./:<=>?@\\^~") <?> "symbolic atom"
    quotedP = (do q <- char '"'
                  s <- manyTill anyChar (try $ char '"')
                  return $ StringLiteral s ) <?> "string literal"

constructorP :: Parser String
constructorP = do
  c1 <- char '\''
  c2 <- upper
  cs <- many (alphaNum <|> char '_')
  return $ (c2:cs)

-- | Parser for a bedelibry prolog variable.
variableP :: Parser String
variableP = (do c <- upper <|> char '_'
                cs <- many (alphaNum <|> char '_')
                return (c:cs)) <?> "variable"
