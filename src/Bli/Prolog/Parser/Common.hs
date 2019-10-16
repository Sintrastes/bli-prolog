
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

emptyListTerm :: Term
emptyListTerm = Comp (ListLiteral []) []

-- Todo: Make this explicitly a partial function and do error handling.
cons :: Term -> Term -> Term
cons (Comp h []) (Comp (ListLiteral ts) []) = Comp (ListLiteral (h:ts)) [] 

parens :: Parser p -> Parser p
parens p = between (csymb '(') (csymb ')') p

identifierP :: Parser String
identifierP =
        (do c <- lower
            cs <- many (alphaNum <|> char '_')
            case (c:cs) of
              "rel"  -> fail "\"rel\" is a reserved keyword, and may not be used as an identifer."
              -- This screws things up by throwing errors here. The logic for not using these
              -- reserved words incorrectly should be implemented somewhere else.
              -- "type" -> fail "\"type\" is a reserved keyword, and may not be used as an identifier."
              -- "entity" -> fail "\"entity\" is a reserved keyword, and may not be used as an identifer."
              otherwise -> return $ (c:cs)) <?> "atom"

intLiteralP :: Parser Atom
intLiteralP = IntLiteral <$> read <$> many1 digit

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
