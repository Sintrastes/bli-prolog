
--
-- | The main parsers for bli prolog files
--

module Bli.Prolog.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Bli.Prolog.Parser.Common
import Data.Bli.Prolog.Ast
import Data.Schema
import Control.Monad (join)

-- | Loads a plain prolog file @filename@, and parses it into a list of clauses.
clausesFromFile filename = parseFromFile programP filename

-- | Parses a plain prolog file directly from a string into a list of clauses.
clausesFromString context = parse programP "" context

-- | Parse a typed bedelibry prolog command from a string.
parseBliCommandTyped string = parse bliCommandTypedP "" string

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

-- | Parser for a pure prolog program. 
programP :: Parser Program
programP = do spacesOrComments
              clauses <- many1 clauseP
              return clauses

-- Typed schema parsing

parseTypedSchemaFile = parseFromFile typedSchemaFileP 
parseTypedSchema = parse typedSchemaFileP ""

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
  id <- atomP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- atomP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id args []

schemaEmptyRelnP :: Parser TypedSchemaEntry
schemaEmptyRelnP = do
  symb "rel"
  id <- atomP
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred NotStored id [] []

schemaStoredRelnP :: Parser TypedSchemaEntry
schemaStoredRelnP = do
  symb "stored"
  symb "rel"
  id <- atomP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- atomP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred Stored id args []

schemaExternalRelnP :: Parser TypedSchemaEntry
schemaExternalRelnP = do
  symb "extern"
  symb "rel"
  id <- atomP
  (csymb ':') <?> "Missing \":\" in relation definition."
  args <- atomP `sepBy1` (csymb ',')
  (csymb '.') <?> "Missing terminating \".\" to relation declaration."
  return $ Pred External id args []

schemaDatatypeDeclP :: Parser TypedSchemaEntry
schemaDatatypeDeclP = do
  symb "datatype"
  typeName <- atomP
  symb "where"
  constructors <- many datatypeConstructorP
  return $ DataType typeName constructors

datatypeConstructorP :: Parser (String, [String])
datatypeConstructorP = do
  symb "constructor"
  constructorName <- constructorP
  csymb ':'
  types <- atomP `sepBy` (csymb ',')
  csymb '.'
  return (constructorName, types)

schemaEntityP :: Parser TypedSchemaEntry
schemaEntityP = do
  id <- atomP
  (csymb ':') <?> "Missing \":\" in entity declaration."
  entityType <- atomP
  (csymb '.') <?> "Missing terminating \".\" to entity declaration."
  return $ TypeOf id entityType

typeDeclP :: Parser TypedSchemaEntry
typeDeclP = do
  symb "type"
  typeId <- atomP
  (csymb '.') <?> "Missing terminating \".\" to type declaration."
  return $ Type typeId

----------- Typed .bpl file parser

parseTypedBliFile = parseFromFile typedBliFileP 
parseTypedBli = parse typedBliFileP ""

typedSchemaLineP :: Parser BliCommandTyped
typedSchemaLineP = do
    res <- (try schemaRelnP <|> try schemaEmptyRelnP 
       <|> try schemaStoredRelnP <|> try schemaEntityP
       <|> try schemaExternalRelnP <|> try typeDeclP 
       <|> schemaDatatypeDeclP)
    return (T_AssertSchema res)

typedBliFileP :: Parser [BliCommandTyped]
typedBliFileP = do
  lines' <- many $ try typedSchemaLineP `eitherP` clauseP
  let lines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> T_AssertClause clause) lines'
  return lines

------------ Typed bli command parser

terminated parser = do
  result <- parser
  eof
  return result

-- | Parser for a bedelibry command.
bliCommandTypedP :: Parser BliCommandTyped
bliCommandTypedP = do 
  result <- (try (terminated typedSchemaLineP) `eitherP` (try (terminated assertClauseP) `eitherP` (try (terminated assertionP) `eitherP` (try (terminated goalP) `eitherP` (terminated lambdaGoalP)))))
  case result of
     Left x  -> return $ x
     Right x -> case x of 
         Left y  -> return $ T_AssertClause y
         Right y -> case y of 
            Left  z -> return $ T_AssertMode z
            Right z -> case z of
                Left w  -> return $ T_QueryMode w
                Right w -> return $ T_LambdaQuery w