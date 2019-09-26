--
-- 
--

module Prolog.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad.Combinators (eitherP)
import Data.Prolog.Ast

---------------------------------------------------------------------
-- Expernal Interface
---------------------------------------------------------------------

clausesFromFile filename = parseFromFile programP filename

clausesFromString context = parse programP context

goalFromString string = parse goalP "<goalstring>" string

lambdaGoalFromString string = parse lambdaGoalP "<goalstring>" string

parseBliCommand string = parse bliCommandP "" string

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


bliCommand :: Parser BliCommand
bliCommand = do 
  result <- (try lambdaGoalP `eitherP` (try assertClauseP `eitherP` (try assertionP `eitherP` goalP))) 
  case result of
     Left x  -> return $ LambdaQuery x
     Right x -> case x of 
         Left y  -> return $ AssertClause y
         Right y -> case y of 
            Left  z -> return $ AssertMode z
            Right z -> return $ QueryMode z
    

lambdaGoalP :: Parser LambdaGoal
lambdaGoalP = do symb "?-"
                skipMany (space >> return ())
                csymb '\\' <|> csymb 'λ' <|> csymb 'Λ'
                vars <- variableP `sepBy` (csymb ',')
                csymb '.'
                ts <- termsP
                csymb '.'
                return (vars, ts)                
                
goalP :: Parser Goal
goalP = do symb "?-"
          ts <- termsP
          csymb '.'
          return ts

assertionP :: Parser Goal
assertionP = do symb "?-"
               ts <- termsP
               csymb '!'
               return ts

 
programP :: Parser Program
programP = do spacesOrComments
             clauses <- many1 clauseP
             return clauses

clauseP :: Parser Clause
clauseP = do t <- term
            body <- option []
                    (symb ":-" >> termsP)
            csymb '.'
            return (t, body)

assertClauseP :: Parser Clause
assertClauseP = do symb "?-"
                  t <- termP
                  body <- option []
                       (symb ":-" >> termsP)
                  csymb '!'
                  return (t, body)

termP :: Parser Term
termP =  (variableP >>= return . Var)
    <|> literalP
    <|> (listP     <?> "list term")


termsP :: Parser Terms
termsP = sepBy1 termP (csymb ',')

literalP :: Parser Term
literalP = do id <- atomP
             option (Comp id [])
                    (parens termsP >>= return . Comp id)
          <|>
          do ts <- parens termsP
             return $ commas ts
               where commas [a] = a
                     commas (h:t) = Comp "," [h, commas t]

parensP :: Parser p -> Parser p
parensP p = between (csymb '(') (csymb ')') p

listP :: Parser Term
listP = between (csymb '[') (csymb ']')
               (option emptyListTermP listTermsP)

listTermsP :: Parser Term
listTermsP =
    do heads <- terms
       tail <- option emptyListTermP
                      (csymb '|' >> term)
       return (foldr consP tail heads)

emptyListTermP :: Term
emptyListTermP = Comp "[]" []

consP :: Term -> Term -> Term
consP h t = Comp "." [h,t]

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

variableP :: Parser String
variableP = (do c <- upper <|> char '_'
               cs <- many (alphaNum <|> char '_')
               return (c:cs)) <?> "variable"
