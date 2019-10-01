--
-- 
--

module Prolog.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Data.Prolog.Ast
import Data.Schema
import Control.Monad (join)

---------------------------------------------------------------------
-- Expernal Interface
---------------------------------------------------------------------

-- | Loads a prolog file @filename@, and parses it into a list of clauses.
clausesFromFile filename = parseFromFile programP filename

-- | Parses a prolog file directly from a string into a list of clauses.
clausesFromString context = parse programP "" context

goalFromString string = parse goalP "<goalstring>" string

lambdaGoalFromString string = parse lambdaGoalP "<goalstring>" string

-- | Parse a bedelibry prolog command from a string
parseBliCommand string = parse bliCommandP "" string

-- | Parse a .bli file
parseBliFile string = parse bliFileP "" string

-- | Parse a .bli file given the path to the file
parseBliFromFile filename = parseFromFile bliFileP filename

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------
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

-- DISCLAIMER (aka TODO): If you are a student looking at this code,
-- you should copy the following two functions, it shouldn't be needed
-- to drop space both before and after a token.
csymb c = (try(spacesOrComments >> char c) >> spacesOrComments)
symb s = (try(spacesOrComments >> string s) >> spacesOrComments)

-- | Parser for a pure prolog program. 
bliFileP :: Parser [BliCommand]
bliFileP = do spacesOrComments
              clauses <- many1 bliFileLineP
              return $ join clauses

bliFileLineP :: Parser [BliCommand]
bliFileLineP = do
  result <- try schemaLineP `eitherP` (try goalP `eitherP` try clauseP)
  case result of
      Left x -> do
          spacesOrComments
          return $ [AssertTypePred x]
      Right x -> case x of
          Left y  -> do
              spacesOrComments
              return $ map (\x -> AssertClause (x,[])) y
          Right y -> do
              spacesOrComments
              return $ [AssertClause y]

-- | Parser for a bedelibry command.
bliCommandP :: Parser BliCommand
bliCommandP = do 
  result <- (try lambdaGoalP `eitherP` (try assertClauseP `eitherP` (try assertionP `eitherP` (try goalP `eitherP` schemaCommandP))))
  case result of
     Left x  -> return $ LambdaQuery x
     Right x -> case x of 
         Left y  -> return $ AssertClause y
         Right y -> case y of 
            Left  z -> return $ AssertMode z
            Right z -> case z of
                Left w  -> return $ QueryMode w
                Right w -> return $ AssertTypePred w
-- | Parser for a lambda query.
lambdaGoalP :: Parser LambdaGoal
lambdaGoalP = do symb "?-"
                 skipMany (space >> return ())
                 csymb '\\' <|> csymb 'λ' <|> csymb 'Λ'
                 vars <- variableP `sepBy` (csymb ',')
                 csymb '.'
                 ts <- termsP
                 csymb '.'
                 return (vars, ts)                

-- | Parser for a plain prolog goal.                
goalP :: Parser Goal
goalP = do symb "?-"
           ts <- termsP
           csymb '.'
           return ts

-- | Parser for an assertion -- a prolog goal ending with a ! instead of a .
assertionP :: Parser Goal
assertionP = do symb "?-"
                ts <- termsP
                csymb '!'
                return ts

-- | Parser for a pure prolog program. 
programP :: Parser Program
programP = do spacesOrComments
              clauses <- many1 clauseP
              return clauses

-- | Parser for a plain prolog clause. Depreciated. 
clauseP :: Parser Clause
clauseP = do t <- termP
             body <- option []
                     (symb ":-" >> termsP)
             csymb '.'
             return (t, body)

-- | Parser for the assertion of a prolog clause.
assertClauseP :: Parser Clause
assertClauseP = do symb "?-"
                   t <- termP
                   body <- option []
                        (symb ":-" >> termsP)
                   csymb '!'
                   return (t, body)

-- | Parser for a prolog term.
termP :: Parser Term
termP =  (variableP >>= return . Var)
    <|> literalP
    <|> (listP     <?> "list term")


-- | Parser for a list of prolog terms.
termsP :: Parser Terms
termsP = sepBy1 termP (csymb ',')

-- | Parser for a prolog literal. (What does this do?)
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

-- | Parser for a bedelibry prolog identifier.
atomP :: Parser Atom
atomP = plain <|> symbolic <|> quoted
  where
    plain = (do c <- lower
                cs <- many (alphaNum <|> char '_')
                return (c:cs)) <?> "atom"
    symbolic = (many1 $ oneOf "#$&*+-./:<=>?@\\^~") <?> "symbolic atom"
    quoted = (do q <- char '\''
                 s <- manyTill anyChar (try $ char '\'')
                 return $ s ) <?> "quoted atom"  -- drop quotes; 'a' == a

-- | Parser for a bedelibry prolog variable.
variableP :: Parser String
variableP = (do c <- upper <|> char '_'
                cs <- many (alphaNum <|> char '_')
                return (c:cs)) <?> "variable"

-- Schema parsing

schemaFromFile :: String -> IO (Either ParseError Schema)
schemaFromFile filepath = parseFromFile schemaFileP filepath

parseSchemaFile :: String -> (Either ParseError Schema)
parseSchemaFile fileContents = parse schemaFileP "" fileContents 

schemaFileP :: Parser Schema
schemaFileP = many schemaLineP

schemaLineP :: Parser (String, Int)
schemaLineP = do
  id <- atomP
  csymb ':'
  arity <- read <$> many1 digit
  ((try $ oneOf "\n") >> return ()) <|> eof
  return (id, arity)
  
schemaCommandP :: Parser (String, Int)
schemaCommandP = do
  symb "?-"
  id <- atomP
  csymb ':'
  arity <- read <$> many1 digit
  return (id, arity)

-- Typed schema parsing

parseTypedSchemaFile = parseFromFile typedSchemaFileP 
parseTypedSchema = parse typedSchemaFileP ""

typedSchemaFileP :: Parser TypedSchema
typedSchemaFileP = do
  lines <- many (try schemaRelnP <|> try schemaEntityP <|> typeDeclP)
  return lines

schemaRelnP :: Parser TypedSchemaEntry
schemaRelnP = do
  symb "rel"
  id <- atomP
  csymb ':'
  args <- atomP `sepBy1` (csymb ',')
  csymb '.'
  return $ Pred id args

schemaEntityP :: Parser TypedSchemaEntry
schemaEntityP = do
  id <- atomP
  csymb ':'
  entityType <- atomP
  csymb '.'
  return $ TypeOf id entityType

typeDeclP :: Parser TypedSchemaEntry
typeDeclP = do
  symb "type"
  typeId <- atomP
  csymb '.'
  return $ Type typeId

----------- Typed .bpl file parser

parseTypedBliFile = parseFromFile typedBliFileP 
parseTypedBli = parse typedBliFileP ""

typedSchemaLineP :: Parser BliCommandTyped
typedSchemaLineP = do
    xxline <- (try schemaRelnP <|> try schemaEntityP <|> typeDeclP)
    return (T_AssertSchema res)

typedBliFileP :: Parser [BliCommandTyped]
typedBliFileP = do
  lines' <- many $ try typedSchemaLineP `eitherP` clauseP
  let lines = map (\line -> case line of
                              Left sEntry  -> sEntry
                              Right clause -> T_AssertClause clause) lines'
  return lines

------------ Typed bli command parser

-- | Parser for a bedelibry command.
bliCommandTypedP :: Parser BliCommandTyped
bliCommandTypedP = do 
  result <- (try lambdaGoalP `eitherP` (try assertClauseP `eitherP` (try assertionP `eitherP` (try goalP `eitherP` typedSchemaLineP))))
  case result of
     Left x  -> return $ LambdaQuery x
     Right x -> case x of 
         Left y  -> return $ AssertClause y
         Right y -> case y of 
            Left  z -> return $ AssertMode z
            Right z -> case z of
                Left w  -> return $ QueryMode w
                Right w -> return $ AssertSchema w