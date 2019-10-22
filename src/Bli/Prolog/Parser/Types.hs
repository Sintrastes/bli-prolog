
module Bli.Prolog.Parser.Types where

typeP :: Parser (BliPrologType a)
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
  
typeVarP :: Parser (BliPrologType Polymorphic)
typeVarP = do
  typeVarId <- variableP
  return $ TypeVar typeVarId

listTypeP :: Parser (BliPrologType a)
listTypeP = do
  symb "list"
  csymb '['
  types <- typeP `sepBy1` (csymb ',')
  csymb ']'
  return

funcTypeP :: Parser (BliPrologType a)
funcTypeP = do
  types <- typeP `sepBy` (try symb "<-" <|> symb "->") 
  return $ foldr' (FuncT RightArr) types

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

goalTypeP :: Parser (BliPrologType a)
goalTypeP = do
  symb "goal"
  csymb '['
  types <- typeP `sepBy1` (csymb ',')
  csymb ']'
  return

ruleTypeP :: Parser (BliPrologType a)
ruleTypeP = do
  symb "rule"
  return RuleT

stringTypeP :: Parser (BliPrologType Monomorphic)
stringTypeP = do
  symb "string"
  return StringT

datetimeTypeP :: Parser (BliPrologType Monomorphic)
datetimeTypeP = do
  symb "datetime"
  return DateTimeT

dateP :: Parser (BliPrologType Monomorphic)
dataP = do
  symb "date"
  return DateP