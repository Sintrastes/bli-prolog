
module Bli.Prolog.Parser.Types where

import Data.Bli.Prolog.Types
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.Util
import Data.BliParser

typeP :: BliParser BliPrologType
typeP = 
     try typeVarP
 <|> try listTypeP
 <|> try funcTypeP
 <|> try entityTypeP
 <|> try stringTypeP
 <|> try intTypeP
 <|> try floatTypeP
 <|> try typTypesP
 <|> try goalTypeP
 <|> try ruleTypeP
 <|> try datetimeTypeP
 <|> try dateTypeP
 <|> declaredTypeP
  
typeVarP :: BliParser BliPrologType
typeVarP = do
  typeVarId <- variableP
  return $ TypeVar typeVarId

listTypeP :: BliParser BliPrologType
listTypeP = do
  symb "list"
  csymb '['
  typ <- typeP
  csymb ']'
  return $ ListT $ typ

funcTypeP :: BliParser BliPrologType
funcTypeP = do
  types <- typeP `sepBy` ((try (symb "<-") <|> symb "->") >> return ())
  return $ foldr1 (FuncT RightArr) $ types

entityTypeP :: BliParser BliPrologType
entityTypeP = do
  symb "entity"
  return EntityT

declaredTypeP :: BliParser BliPrologType
declaredTypeP = do
  id <- identifierP
  return $ DeclaredTypeT id

typTypesP :: BliParser BliPrologType
typTypesP = do
  symb "type"
  return TypTypesT

goalTypeP :: BliParser BliPrologType
goalTypeP = do
  symb "goal"
  csymb '['
  types <- typeP `sepBy1` (csymb ',')
  csymb ']'
  return $ GoalT types

ruleTypeP :: BliParser BliPrologType
ruleTypeP = do
  symb "rule"
  return RuleT

intTypeP :: BliParser BliPrologType
intTypeP = do
  symb "int"
  return IntLitT

floatTypeP :: BliParser BliPrologType
floatTypeP = do
  symb "float"
  return FloatLitT

stringTypeP :: BliParser BliPrologType
stringTypeP = do
  symb "string"
  return StringLitT

datetimeTypeP :: BliParser BliPrologType
datetimeTypeP = do
  symb "datetime"
  return DateTimeLitT

dateTypeP :: BliParser BliPrologType
dateTypeP = do
  symb "date"
  return DateLitT