
--
-- | Parsers for generic prolog components (like atoms) which are used
--   by other more specifc parsers.

module Bli.Prolog.Parser.Common where

import Data.Char
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Data.Bli.Prolog.Ast
import Bli.Prolog.Parser.DateTime
import Bli.Prolog.Parser.Util
import Data.BliParser
import Bli.App.Config.Features
import Bli.App.Config
-- import Bli.Prolog.Parser.Datatypes

emptyListTerm :: Term
emptyListTerm = Comp (ListLiteral []) []

-- Todo: Make this explicitly a partial function and do error handling.
cons :: Term -> Term -> Term
cons (Comp h []) (Comp (ListLiteral ts) []) = Comp (ListLiteral (h:ts)) [] 

-- Operators, which wil be parsed as infix operators by the parser.
operatorP :: BliParser String
operatorP = try (symb "is" >> return "is") 
  <|> try (do csymb '`'
              id <- identifierP
              csymb '`'
              return id)
  -- Note: We probably shouldn't allow e.x. : and . by themselves,
  -- as this could mess with parsing -- but we could allow them to be used
  -- in certain contexts. But for now, we won't use them at all.
  <|> many (oneOf "#¿¡$&*+-/;<=≌>?@\\⊦⊨⊞⋆∗∘∙⋅⊟⊠∧∨×⊙⊘^~⊗⊕∩∖∪⨝∈≺≻≼≽⊏⊐⊑⊒⊓⊔←→⟵⟶⟷↼⇀↽⇁⇸")

-- Identifiers, which will be parsed as either terms or predicates.
identifierP :: BliParser String
identifierP =
        (do c <- lower <|> ifEnabledP UnicodeSyntax 
                             (char '☐' <|> char '◇' 
                          <|> char '■' <|> char '◆'
                          <|> char '∅' <|> char 'ℕ'
                          <|> char 'ℤ' <|> char 'ℚ'
                          <|> char 'ℝ' <|> char 'ℂ'
                          <|> char '¬' <|> char '∀'
                          <|> char '∃' <|> char '⊥'
                          <|> char '⊤' <|> char '⟡'
                          <|> char '↑' <|> char '↓'
                          <|> char '↿' <|> char '↾'
                          <|> char '⇃' <|> char '⇂'
                          <|> char '※' <|> char '±'
                          <|> char '∓' <|> char '‽'
                          <|> char '⸮'
                          <|> char '⁂' <|> char '♭'
                          <|> char '♮' <|> char '♯')
            cs <- many (alphaNum <|> char '_')
            case (c:cs) of
              "rel"  -> fail "\"rel\" is a reserved keyword, and may not be used as an identifer."
              -- This screws things up by throwing errors here. The logic for not using these
              -- reserved words incorrectly should be implemented somewhere else.
              -- "typel" -> fail "\"type\" is a reserved keyword, and may not be used as an identifier."
              -- "entity" -> fail "\"entity\" is a reserved keyword, and may not be used as an identifer."
              otherwise -> return $ (c:cs)) <?> "atom"

intLiteralP :: BliParser Atom
intLiteralP = IntLiteral <$> read <$> many1 digit

floatLiteralP :: BliParser Atom
floatLiteralP = do 
  integralPart <- many1 digit
  char '.'
  decimalPart <- many digit
  return $ FloatLiteral $ (read (integralPart ++ "." ++ decimalPart) :: Double)

constructorP :: BliParser String
constructorP = do
  c1 <- char '\''
  c2 <- upper
  cs <- many (alphaNum <|> char '_')
  return $ (c2:cs)

-- | Parser for a bedelibry prolog variable.
variableP :: BliParser String
variableP = (do c <- upper <|> char '_'
                cs <- many (alphaNum <|> char '_')
                if isAscii c 
                then return (c:cs)
                else fail "Variables many not use unicode.") <?> "variable"


