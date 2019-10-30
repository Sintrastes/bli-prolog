
--
-- | Utilities for building parsers.
--

module Bli.Prolog.Parser.Util where

import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Data.BliParser
import Control.Monad

-- | Helper function for a multi-character symbol which can be
--   surrounded by whitespace.
symb :: String -> BliParser ()
symb s = (try (spacesOrComments >> string s) >> spacesOrComments)

-- | Helper function to generate parsers which
--   are terminated by eof.
terminated :: BliParser a -> BliParser a
terminated parser = do
  result <- parser
  eof
  return result

-- | Parser for comments in a prolog file
commentP :: BliParser ()
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

parens :: BliParser a -> BliParser a
parens p = between (csymb '(') (csymb ')') p

spacesOrComments :: BliParser ()
spacesOrComments = skipMany ((space >> return ()) <|> commentP)

-- | Helper function for a symbol consisting of a single character
--   which can be surrounded by whitespace.
csymb :: Char -> BliParser ()
csymb c = (try(spacesOrComments >> char c) >> spacesOrComments)
