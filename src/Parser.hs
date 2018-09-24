module Parser
       ( clausesFromFile
       , clausesFromString
       , goalFromString
       )
where

import Text.ParserCombinators.Parsec

import Ast

---------------------------------------------------------------------
-- Expernal Interface
---------------------------------------------------------------------

clausesFromFile filename = parseFromFile program filename

clausesFromString context = parse program context

goalFromString string = parse goal "<goalstring>" string



----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------
comment = singleLineComment <|> multiLineComment
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


spacesOrComments = skipMany ((space >> return()) <|> comment)



-- DISCLAIMER (aka TODO): If you are a student looking at this code,
-- you should copy the following two functions, it shouldn't be needed
-- to drop space both before and after a token.
csymb c = (try(spacesOrComments >> char c) >> spacesOrComments)
symb s = (try(spacesOrComments >> string s) >> spacesOrComments)

goal :: Parser Goal
goal = do symb "?-"
          ts <- terms
          csymb '.'
          return ts

program :: Parser Program
program = do spacesOrComments
             clauses <- many1 clause
             return clauses

clause :: Parser Clause
clause = do t <- term
            body <- option []
                    (symb ":-" >> terms)
            csymb '.'
            return (t, body)

term :: Parser Term
term =  (variable >>= return . Var)
    <|> literal
    <|> (list     <?> "list term")


terms :: Parser Terms
terms = sepBy1 term (csymb ',')

literal :: Parser Term
literal = do id <- atom
             option (Comp id [])
                    (parens terms >>= return . Comp id)
          <|>
          do ts <- parens terms
             return $ commas ts
               where commas [a] = a
                     commas (h:t) = Comp "," [h, commas t]

parens :: Parser p -> Parser p
parens p = between (csymb '(') (csymb ')') p

list :: Parser Term
list = between (csymb '[') (csymb ']')
               (option emptyListTerm listTerms)

listTerms :: Parser Term
listTerms =
    do heads <- terms
       tail <- option emptyListTerm
                      (csymb '|' >> term)
       return (foldr cons tail heads)

emptyListTerm :: Term
emptyListTerm = Comp "[]" []

cons :: Term -> Term -> Term
cons h t = Comp "." [h,t]

atom :: Parser Atom
atom = plain <|> symbolic <|> quoted
  where
    plain = (do c <- lower
                cs <- many (alphaNum <|> char '_')
                return (c:cs)) <?> "atom"
    symbolic = (many1 $ oneOf "#$&*+-./:<=>?@\\^~") <?> "symbolic atom"
    quoted = (do q <- char '\''
                 s <- manyTill anyChar (try $ char '\'')
                 return $ s ) <?> "quoted atom"  -- drop quotes; 'a' == a

variable :: Parser String
variable = (do c <- upper <|> char '_'
               cs <- many (alphaNum <|> char '_')
               return (c:cs)) <?> "variable"
