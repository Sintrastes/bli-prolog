
module Bli.Prolog.Parser.Types where

import Data.Bli.Prolog.Types
import Text.ParserCombinators.Parsec
import Bli.Prolog.Parser.Common

typeP :: Parser BliPrologType
typeP = 
     try typeVarP
 <|> try listTypeP
 <|> try funcTypeP
 <|> try entityTypeP
 <|> try declaredTypeP
 <|> try typTypesP
 <|> try goalTypeP
 <|> try ruleTypeP
 <|> try stringTypeP
 <|> try datetimeTypeP
 <|> try dateTypeP
  
typeVarP :: Parser BliPrologType
typeVarP = do
  typeVarId <- variableP
  return $ TypeVar typeVarId

listTypeP :: Parser BliPrologType
listTypeP = do
  symb "list"
  csymb '['
  typ <- typeP
  csymb ']'
  return $ ListT $ typ

funcTypeP :: Parser BliPrologType
funcTypeP = do
  types <- typeP `sepBy` ((try (symb "<-") <|> symb "->") >> return ())
  return $ foldr1 (FuncT RightArr) $ types

entityTypeP :: Parser BliPrologType
entityTypeP = do
  symb "entity"
  return EntityT

declaredTypeP :: Parser BliPrologType
declaredTypeP = do
  id <- identifierP
  return $ DeclaredTypeT id

typTypesP :: Parser BliPrologType
typTypesP = do
  symb "type"
  return TypTypesT

goalTypeP :: Parser BliPrologType
goalTypeP = do
  symb "goal"
  csymb '['
  types <- typeP `sepBy1` (csymb ',')
  csymb ']'
  return $ GoalT types

ruleTypeP :: Parser BliPrologType
ruleTypeP = do
  symb "rule"
  return RuleT

stringTypeP :: Parser BliPrologType
stringTypeP = do
  symb "string"
  return StringLitT

datetimeTypeP :: Parser BliPrologType
datetimeTypeP = do
  symb "datetime"
  return DateTimeLitT

dateTypeP :: Parser BliPrologType
dateTypeP = do
  symb "date"
  return DateLitT