
--
-- | Parsers for schema files
-- 


module Bli.Prolog.Parser.Schema where

import Bli.Prolog.Parser.Common
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import Control.Monad.Combinators (eitherP)
import Data.Bli.Prolog.Ast
import Data.Bli.Prolog.Schema
import Bli.App.Config.Features
import Bli.Prolog.Parser.Util
import Data.BliParser
import Control.Monad (join)
import Bli.App.Config

import Debug.Trace

-- Depreciated:
-- parseTypedSchemaFile = parseFromFile typedSchemaFileP 
parseTypedSchema = parseBli typedSchemaFileP

typedSchemaLineP :: BliParser BliCommand
typedSchemaLineP = trace "in typed schema line" $ do
    many space
    res <- (try schemaRelnP 
       <|> try schemaEmptyRelnP 
       <|> try schemaStoredRelnP
       <|> try usingDeclP
       <|> try schemaEntityP
       <|> try schemaExternalRelnP
       <|> try typeDeclP 
       <|> try (ifEnabledP BedelibryInteraction typeDeclExternP)
       <|> try schemaEntityRelnP
       <|> try schemaDatatypeDeclP
       <|> schemaExternalRelnHSP)
    return (AssertSchema res)

typedSchemaFileP :: BliParser Schema
typedSchemaFileP = do
  features <- join <$> many (try featureDeclP <|> featureDisableDeclP)
  lines <- many (try schemaRelnP <|> try schemaEntityP <|> typeDeclP)
  return (features++lines)

-- | Parser for language feature declarations.
featureDeclP :: BliParser [SchemaEntry]
featureDeclP = do
  symb "/*"
  symb "LANGUAGE"
  featureName <- variableP `sepBy1` (csymb ',')
  symb "*/"
  return $ map (Feature True) $ (map read featureName :: [LanguageOption])

-- | Parser for language feature declarations.
featureDisableDeclP :: BliParser [SchemaEntry]
featureDisableDeclP = do
  symb "/*"
  symb "LANGUAGE"
  symb "DISABLE"
  featureName <- variableP `sepBy1` (csymb ',')
  symb "*/"
  return $ map (Feature False) $ (map read featureName :: [LanguageOption])
  
-- | A parser for "using" import statements in .bpl and .bsc files.
usingDeclP :: BliParser SchemaEntry
usingDeclP = do
  symb "using"
  mod <- many (oneOf ['a'..'z'] <|> char '_')
  csymb '.'
  return $ Using mod

schemaRelnP :: BliParser SchemaEntry
schemaRelnP = do
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id args []

-- | Syntatic sugar for (e.x) rel p: entity, entity, entity.
--   using the Prolog notation: p/3.
schemaEntityRelnP :: BliParser SchemaEntry
schemaEntityRelnP = do
  symb "rel"
  id <- identifierP
  char '/'
  n <- read <$> many1 digit
  let args = replicate n "entity"
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id args []

schemaEmptyRelnP :: BliParser SchemaEntry
schemaEmptyRelnP = do
  symb "rel"
  id <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id [] []

schemaStoredRelnP :: BliParser SchemaEntry
schemaStoredRelnP = do
  symb "stored"
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred Stored id args []

schemaExternalRelnP :: BliParser SchemaEntry
schemaExternalRelnP = do
  symb "extern"
  symb "rel"
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- identifierP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred External id args []

schemaExternalRelnHSP :: BliParser SchemaEntry
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

schemaDatatypeDeclP :: BliParser SchemaEntry
schemaDatatypeDeclP = do
  symb "datatype"
  typeName <- identifierP
  symb "where"
  constructors <- many (try datatypeConstructorEmptyP <|> datatypeConstructorP)
  return $ DataType typeName constructors

schemaDatatypeDeclWithInheritanceP :: BliParser SchemaEntry
schemaDatatypeDeclWithInheritanceP = do
  symb "datatype"
  typeName <- identifierP
  symb "where"
  constructors <- many (try datatypeConstructorEmptyP <|> datatypeConstructorP)
  symb "extends"
  parentTypeNames <- many1 identifierP
  -- I think I need a new case for this -- I'm not sure what to do with
  -- parentTypeNames
  return $ DataType typeName constructors

datatypeConstructorEmptyP :: BliParser (String, [String])
datatypeConstructorEmptyP = do
  symb "constructor"
  constructorName <- constructorP
  csymb '.'
  return (constructorName, [])

datatypeConstructorP :: BliParser (String, [String])
datatypeConstructorP = do
  symb "constructor"
  constructorName <- constructorP
  csymb ':'
  types <- identifierP `sepBy` (csymb ',')
  csymb '.'
  return (constructorName, types)

schemaEntityP :: BliParser SchemaEntry
schemaEntityP = do
  id <- identifierP
  (csymb ':') <?> "Missing \":\" in entity declaration."
  entityType <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to entity declaration."
  return $ TypeOf id entityType

-- | Declares a (local) type.
typeDeclP :: BliParser SchemaEntry
typeDeclP = do
  symb "type"
  typeId <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to type declaration."
  return $ Type False typeId

-- | Declares an external type -- that is, one that is
--   handled by an external bedelibry server.
typeDeclExternP :: BliParser SchemaEntry
typeDeclExternP = do
  symb "extern"
  symb "type"
  typeId <- identifierP
  (csymb '.') <?> "Missing terminating \".\" to type declaration."
  return $ Type True typeId
