
--
-- | Parsers for schema files
-- 


module Bli.Prolog.Parser.Schema where

import Bli.Prolog.Parser.Common
import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema

parseTypedSchemaFile = parseFromFile typedSchemaFileP 
parseTypedSchema = parse typedSchemaFileP ""

typedSchemaLineP :: Parser BliCommandTyped
typedSchemaLineP = do
    res <- (try schemaRelnP 
       <|> try schemaEmptyRelnP 
       <|> try schemaStoredRelnP
       <|> try schemaEntityP
       <|> try schemaExternalRelnP
       <|> try typeDeclP 
       <|> try schemaDatatypeDeclP
       <|> schemaExternalRelnHSP)
    return (T_AssertSchema res)

typedSchemaFileP :: Parser TypedSchema
typedSchemaFileP = do
  lines <- many (try schemaRelnP <|> try schemaEntityP <|> typeDeclP)
  return lines

-- | A parser for "using" import statements in .bpl and .bsc files.
usingDeclP :: Parser TypedSchemaEntry
usingDeclP = do
  symb "using"
  mod <- many (oneOf ['a'..'z'] <|> char '_')
  csymb '.'
  return $ Using mod

schemaRelnP :: Parser TypedSchemaEntry
schemaRelnP = do
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id args []

schemaEmptyRelnP :: Parser TypedSchemaEntry
schemaEmptyRelnP = do
  symb "rel"
  id <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id [] []

schemaStoredRelnP :: Parser TypedSchemaEntry
schemaStoredRelnP = do
  symb "stored"
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred Stored id args []

schemaExternalRelnP :: Parser TypedSchemaEntry
schemaExternalRelnP = do
  symb "extern"
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred External id args []

schemaExternalRelnHSP :: Parser TypedSchemaEntry
schemaExternalRelnHSP = do
  symb "extern"
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  symb "in"
  c  <- upper
  cs <- many $ alphaNum
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred (ExternalHS (c:cs)) id args []

schemaDatatypeDeclP :: Parser TypedSchemaEntry
schemaDatatypeDeclP = do
  symb "datatype"
  typeName <- identifierP
  symb "where"
  constructors <- many datatypeConstructorP
  return $ DataType typeName constructors

datatypeConstructorP :: Parser (String, [String])
datatypeConstructorP = do
  symb "constructor"
  constructorName <- constructorP
  csymb ':'
  types <- identifierP `sepBy` (csymb ',')
  csymb '.'
  return (constructorName, types)

schemaEntityP :: Parser TypedSchemaEntry
schemaEntityP = do
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in entity declaration."
  entityType <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to entity declaration."
  return $ TypeOf id entityType

typeDeclP :: Parser TypedSchemaEntry
typeDeclP = do
  symb "type"
  typeId <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to type declaration."
  return $ Type typeId
