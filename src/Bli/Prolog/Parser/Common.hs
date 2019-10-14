
--
-- | Parsers for generic prolog components (like atoms) which are used
--   by other more specifc parsers.

module Bli.Prolog.Parser.Common where

import Text.ParserCombinators.Parsec
import Data.Bli.Prolog.Ast

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
               (option emptyListTermP listTermsP)

listTermsP :: Parser Term
listTermsP =
    do heads <- termsP
       tail <- option emptyListTermP
                      (csymb '|' >> termP)
       return (foldr consP tail heads)

emptyListTermP :: Term
emptyListTermP = Comp "[]" []

consP :: Term -> Term -> Term
consP h t = Comp "." [h,t]

-- | Parser for a list of prolog terms.
termsP :: Parser Terms
termsP = sepBy1 termP (csymb ',')

-- | Parser for a prolog literal. (i.e. not a list term)
literalP :: Parser Term
literalP = do id <- atomP
              option (Comp id [])
                     (parens termsP >>= return . Comp id)
           <|>
           do ts <- parens termsP
              return $ commas ts
                where commas [a] = a
                      commas (h:t) = Comp "," [h, commas t]

parens :: Parser p -> Parser p
parens p = between (csymb '(') (csymb ')') p


-- | Parser for a bedelibry prolog identifier.
atomP :: Parser Atom
atomP = plain <|> symbolic <|> quoted
  where
    plain = (do c <- lower
                cs <- many (alphaNum <|> char '_')
                case (c:cs) of
                  "rel"  -> fail "\"rel\" is a reserved keyword, and may not be used as an identifer."
                  "type" -> fail "\"type\" is a reserved keyword, and may not be used as an identifier."
                  "entity" -> fail "\"entity\" is a reserved keyword, and may not be used as an identifer."
                  otherwise -> return (c:cs)) <?> "atom"
    symbolic = (many1 $ oneOf "#$&*+-./:<=>?@\\^~") <?> "symbolic atom"
    quoted = (do q <- char '"'
                 s <- manyTill anyChar (try $ char '"')
                 return $ "\""++s++"\"" ) <?> "string literal"

constructorP :: Parser String
constructorP = do
  c1 <- char '\''
  c2 <- upper
  cs <- many (alphaNum <|> char '_')
  return (c1:c2:cs)

-- | Parser for a bedelibry prolog variable.
variableP :: Parser String
variableP = (do c <- upper <|> char '_'
                cs <- many (alphaNum <|> char '_')
                return (c:cs)) <?> "variable"
