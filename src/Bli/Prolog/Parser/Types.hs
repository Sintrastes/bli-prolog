
module Bli.Prolog.Parser.Types where

import Data.Bli.Prolog.Types
import Text.ParserCombinators.Parsec
import Bli.Prolog.Parser.Common

typeP :: Parser SomeBliPrologType
typeP = 
     try (packType <$> typeVarP)
 <|> try (listTypeP)
 <|> try (funcTypeP)
 <|> try (packType <$> entityTypeP)
 <|> try (packType <$> declaredTypeP)
 <|> try (packType <$> typTypesP)
 <|> try (goalTypeP)
 <|> try (packType <$> ruleTypeP)
 <|> try (packType <$> stringTypeP)
 <|> try (packType <$> datetimeTypeP)
 <|> try (packType <$> dateTypeP)
  
typeVarP :: Parser (BliPrologType Polymorphic)
typeVarP = do
  typeVarId <- variableP
  return $ TypeVar typeVarId

listTypeP :: Parser SomeBliPrologType
listTypeP = do
  symb "list"
  csymb '['
  typ <- typeP
  csymb ']'
  return $ packType $ ListT $ head $ combineList [typ]

funcTypeP :: Parser SomeBliPrologType
funcTypeP = do
  types <- typeP `sepBy` ((try (symb "<-") <|> symb "->") >> return ())
  return $ packType $ foldr1 (FuncT RightArr) $ combineList types

entityTypeP :: Parser (BliPrologType Monomorphic)
entityTypeP = do
  symb "entity"
  return EntityT

declaredTypeP :: Parser (BliPrologType Monomorphic)
declaredTypeP = do
  id <- identifierP
  return $ DeclaredTypeT id

typTypesP :: Parser (BliPrologType Monomorphic)
typTypesP = do
  symb "type"
  return TypTypesT

goalTypeP :: Parser SomeBliPrologType
goalTypeP = do
  symb "goal"
  csymb '['
  types <- typeP `sepBy1` (csymb ',')
  csymb ']'
  return $ SomeBliPrologType $ GoalT $ combineList types

ruleTypeP :: Parser (BliPrologType Monomorphic)
ruleTypeP = do
  symb "rule"
  return RuleT

stringTypeP :: Parser (BliPrologType Monomorphic)
stringTypeP = do
  symb "string"
  return StringLitT

datetimeTypeP :: Parser (BliPrologType Monomorphic)
datetimeTypeP = do
  symb "datetime"
  return DateTimeLitT

dateTypeP :: Parser (BliPrologType Monomorphic)
dateTypeP = do
  symb "date"
  return DateLitT