
--
-- | Parsers for generic prolog components (like atoms) which are used
--   by other more specifc parsers.

module Bli.Prolog.Parser.Common where

import Text.ParserCombinators.Parsec
import Data.Bli.Prolog.Ast
import Bli.Prolog.Parser.DateTime
import Bli.Prolog.Parser.Util
-- import Bli.Prolog.Parser.Datatypes

emptyListTerm :: Term
emptyListTerm = Comp (ListLiteral []) []

-- Todo: Make this explicitly a partial function and do error handling.
cons :: Term -> Term -> Term
cons (Comp h []) (Comp (ListLiteral ts) []) = Comp (ListLiteral (h:ts)) [] 

-- Operators, which wil be parsed as infix operators by the parser.
operatorP :: Parser String
operatorP = many (oneOf "#¿¡$&*+-./:<=≌>?@\\⊦⊨⊞⋆∗∘∙⋅⊟⊠∧∨×⊙⊘^~⊗⊕∩∖∪⨝∈≺≻≼≽⊏⊐⊑⊒⊓⊔←→⟵⟶⟷↼⇀↽⇁⇸")

-- Identifiers, which will be parsed as either terms or predicates.
identifierP :: Parser String
identifierP =
        (do c <- lower <|> char '☐' <|> char '◇' 
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
                       <|> char '♮' <|> char '♯'
            cs <- many (alphaNum <|> char '_')
            case (c:cs) of
              "rel"  -> fail "\"rel\" is a reserved keyword, and may not be used as an identifer."
              -- This screws things up by throwing errors here. The logic for not using these
              -- reserved words incorrectly should be implemented somewhere else.
              -- "typel" -> fail "\"type\" is a reserved keyword, and may not be used as an identifier."
              -- "entity" -> fail "\"entity\" is a reserved keyword, and may not be used as an identifer."
              otherwise -> return $ (c:cs)) <?> "atom"

intLiteralP :: Parser Atom
intLiteralP = IntLiteral <$> read <$> many1 digit
floatLiteralP :: Parser Atom
floatLiteralP = do 
  integralPart <- many1 digit
  char '.'
  decimalPart <- many digit
  return $ FloatLiteral $ (read (integralPart ++ "." ++ decimalPart) :: Double)

constructorP :: Parser String
constructorP = do
  c1 <- char '\''
  c2 <- upper
  cs <- many (alphaNum <|> char '_')
  return $ (c2:cs)

-- | Parser for a bedelibry prolog variable.
variableP :: Parser String
variableP = (do c <- upper <|> char '_'
                cs <- many (alphaNum <|> char '_')
                return (c:cs)) <?> "variable"


