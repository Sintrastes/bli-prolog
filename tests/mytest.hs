import Parser
import Interp
import Ast
import Data.List (sort)
import System.IO.Unsafe

queryFile :: String -> String -> (SearchTree -> [Solution]) -> IO (Maybe [Solution])
queryFile filename goalString search = do
  clauses <- clausesFromFile filename
  let goal = goalFromString goalString
  case (clauses, goal) of
    (Right p, Right g) -> do
       print (p == clauses2)
       print (g == testGoal2)
       print testGoal2
       print g
       let t = makeReportTree p g
       return $ Just $ search t
    (Left err, _) ->
      return $ Nothing
    (_, Left err) ->
      return $ Nothing

lambdaQueryFile :: String -> String -> (SearchTree -> [Solution]) -> IO (Maybe [Solution])
lambdaQueryFile filename goalString search = do
  clauses <- clausesFromFile filename
  let goal = lambdaGoalFromString goalString
  case (clauses, goal) of
    (Right p, Right (vars, g) ) -> do
       print (p == clauses2)
       print (g == testGoal2)
       print testGoal2
       print g
       let t = makeReportTree p g
       return $ Just 
              $ map Solution 
              $ map (filter (\(x,y) -> x `elem` vars)) 
              $ map (\(Solution x) -> x) $ search t
    (Left err, _) ->
      return $ Nothing
    (_, Left err) ->
      return $ Nothing


-- fromRight (Right x) = x
siblings = queryFile "./tests/siblings.pl" "?- sibling(homer, X)."
testGoal = goalFromString "?- sibling(homer, X)."

testGoal2 = [Comp "sibling" [Comp "homer" [],Var "X"]] 

clauses = unsafePerformIO $ clausesFromFile "./tests/siblings.pl"

clauses2 = [(Comp "member" [Var "X",Comp "." [Var "X",Var "_"]],[]),(Comp "member" [Var "X",Comp "." [Var "_",Var "T"]],[Comp "member" [Var "X",Var "T"]]),(Comp "mother_children" [Comp "marge" [],Comp "." [Comp "bart" [],Comp "." [Comp "lisa" [],Comp "." [Comp "maggie" [],Comp "[]" []]]]],[]),(Comp "mother_children" [Comp "mona" [],Comp "." [Comp "homer" [],Comp "." [Comp "jay" [],Comp "[]" []]]],[]),(Comp "mother_child" [Var "X",Var "Y"],[Comp "mother_children" [Var "X",Var "C"],Comp "member" [Var "Y",Var "C"]]),(Comp "father_child" [Comp "homer" [],Comp "bart" []],[]),(Comp "father_child" [Comp "homer" [],Comp "lisa" []],[]),(Comp "father_child" [Comp "homer" [],Comp "maggie" []],[]),(Comp "father_child" [Comp "abe" [],Comp "homer" []],[]),(Comp "father_child" [Comp "abe" [],Comp "herb" []],[]),(Comp "father_child" [Comp "abe" [],Comp "abbie" []],[]),(Comp "sibling" [Var "X",Var "Y"],[Comp "parent_child" [Var "Z",Var "X"],Comp "parent_child" [Var "Z",Var "Y"]]),(Comp "parent_child" [Var "X",Var "Y"],[Comp "father_child" [Var "X",Var "Y"]]),(Comp "parent_child" [Var "X",Var "Y"],[Comp "mother_child" [Var "X",Var "Y"]])]

result2 = bfs $ makeReportTree clauses2 testGoal2

schema = [("programming_language",1), ("name", 2)]

-- Helper functions for dealing with terms
arity :: Term -> Int
arity (Comp _ xs) = length xs
arity (Var _) = 0

name :: Term -> String
name (Comp s _) = s
name (Var s) = s

-- Function to preform our syntatic sugar expansion
expandTerm :: Term -> [(String,Int)] -> Term
expandTerm term schema = 
  if ((\(x,y) -> (x,y+1))  (name term, arity term)) `elem` schema
  then (\(Comp s xs) -> Comp s ((Var "X"):xs) ) term
  else term 

expandTerms = map expandTerm

main = do
  result <- queryFile "./tests/siblings.pl" "?- sibling(homer, X)." bfs
  print result
  print result2
  result3 <- queryFile "./tests/prog.pl" "?- programming_language(X), name(X,Y)." bfs
  result4 <- lambdaQueryFile "./tests/prog.pl" "?- \\Y. programming_language(X), name(X, Y)." bfs
  print result3
  print result4