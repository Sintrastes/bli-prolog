
module Schema.Parser where

import Text.ParserCombinators.Parsec
import Data.Schema
import Prolog.Parser
import Control.Applicative ((<$>))

schemaFile :: Parser Schema
schemaFile = many line

line :: Parser (String, Int)
line = do
  id <- atomP
  csymb ':'
  arity <- read <$> many1 digit
  _ <- oneOf "\n"
  return (id, arity)
  
  