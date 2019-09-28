
module Schema.Parser where

import Text.ParserCombinators.Parsec
import Data.Schema
import Prolog.Parser
import Control.Applicative ((<$>))

schemaFromFile :: String -> IO (Either ParseError Schema)
schemaFromFile filepath = parseFromFile schemaFile filepath 

schemaFile :: Parser Schema
schemaFile = many line

line :: Parser (String, Int)
line = do
  id <- atomP
  csymb ':'
  arity <- read <$> many1 digit
  ((try $ oneOf "\n") >> return ()) <|> eof
  return (id, arity)
  
schemaCommandP :: Parser (String, Int)
schemaCommandP = do
  id <- atomP
  csymb ':'
  arity <- read <$> many1 digit
  csymb '!'
  return (id, arity)
  