{-
  A basic parser and interpreter for a cut-down version of Prolog

  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module Interp where

import Prelude hiding ((<>))

import Data.List (nub)
import Control.Monad(liftM)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>),(<+>))
import qualified Data.Char as C


import Ast
import Parser

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

type Unifier = [(Variable, Term)]

compose :: Unifier -> Unifier -> Unifier
compose u1 u2 = (map (\(v, t) -> (v, subs u2 t)) u1) ++ u2

occursIn :: Variable -> Term -> Bool
occursIn v (Var x)     = v == x
occursIn v (Comp _ ms) = any (occursIn v) ms


subs :: Unifier -> Term -> Term
subs u t@(Var x)   = maybe t id (lookup x u)
subs u (Comp n ts) = Comp n (map (subs u) ts)

unify :: Term -> Term -> Maybe Unifier
unify (Var "_") t                      = return []
unify t (Var "_")                      = return []
unify (Var x) (Var y) | x == y         = return []
unify (Var x) t | not(x `occursIn` t)  = return [(x, t)]
unify t v@(Var _)                      = unify v t
unify (Comp m ms) (Comp n ns) | m == n = unifyList ms ns
unify _ _                              = Nothing

unifyList (t : ts) (r : rs) =
    do u1 <- unify t r
       u2 <- unifyList (map (subs u1) ts) (map (subs u1) rs)
       return $ u1 `compose` u2
unifyList [] [] = Just []
unifyList _ _   = Nothing

variables :: Terms -> [Variable]
variables ts = nub $ varsList ts
    where vars (Var "_") = []
          vars (Var x) = [x]
          vars (Comp _ ts) = varsList ts
          varsList ts = [ v | t <- ts, v <- vars t]

freshen bound (tc, tb) = (subs sub tc, map (subs sub) tb)
    where vars = variables(tc : tb)
          sub = [ (v, Var $ nextVar 0 v) | v <- vars, v `elem` bound]
          nextVar i v = let v' = "_" ++ show i ++ "_" ++ v in
                        if v' `elem` bound then nextVar (i+1) v
                        else v'

newtype Solution = Solution [(Variable, Term)]
                 deriving (Eq, Read)

instance Show Solution where
  show (Solution bindings) = PP.render(renderB bindings)
    where
      renderB [] = PP.text "true"
      renderB bindings = PP.braces $ PP.vcat $ map renderBindings bindings

      renderBindings (var, term) = PP.text var <+> PP.equals <+> renderT term

      renderAtom a = if isPlain a then PP.text a
                     else PP.text "'" <> PP.text a <> PP.text "'"

      renderT (Var v) = PP.text v
      renderT (Comp a []) = renderAtom a
      renderT comp@(Comp f args) =
        case listTerm comp of
          Just tt -> PP.brackets $ renderTerms tt
          Nothing -> renderAtom f <> (PP.parens $ renderTerms args)

      renderTerms terms = PP.sep $ PP.punctuate PP.comma $ map renderT terms

      listTerm (Comp "[]" [])    = return []
      listTerm (Comp "." [h, t]) = do tt <- listTerm t
                                      return $ h:tt
      listTerm _                 = Nothing

isPlain (c:cs) = C.isLower c && all (\c -> c == '_' || C.isAlphaNum c) cs
isPlain _ = False

data SearchTree = Sol Solution
                | Node Goal [SearchTree]
                  deriving (Eq, Show, Read)

-- Uses the List monad for backtracking
solve :: Program -> Goal -> [SearchTree]
solve _ [r] | isReportGoal r =  return $ Sol $ getSolution r
solve prog g@(t1 : ts) = return $ Node g trees
    where trees = do c <- prog
                     let (tc, tsc) = freshen (variables g) c
                     case unify tc t1 of
                       Just u -> do
                         let g' = map (subs u) $ tsc ++ ts
                         solve prog g'
                       Nothing -> []
--solve _ _ = []

makeReportGoal goal = [Comp "_report" reportVars]
    where reportVars = map (\ v -> Comp "=" [Comp v [], Var v]) vars
          vars = variables goal

isReportGoal (Comp "_report" _) = True
isReportGoal _                  = False

getSolution (Comp "_report" args) = Solution sol
    where sol = filter nontriv $ map (\ (Comp "=" [Comp v [], t]) -> (v, t)) args
          nontriv (x, (Var y)) | x == y = False
          nontriv _ = True
getSolution _ = error "getSolution should never be called like this"

-- Use the trick of inserting an extra reporting goal
makeReportTree prog goal = Node goal $ solve prog (goal ++ makeReportGoal goal)


----------------------------------------------------------------------
-- Traveral of Search Trees
----------------------------------------------------------------------

-- Depth first
dfs :: SearchTree -> [Solution]
dfs (Sol sols) = [sols]
dfs (Node _ st) = [ s | t <- st, s <- dfs t]

-- Breath first
bfs :: SearchTree -> [Solution]
bfs t = trav [t]
    where trav [] = []
          trav ((Sol x) : q) = x : trav q
          trav ((Node _ st)  : q) = trav (q ++ st)

-- Limited depth first
limitedDfs :: Int -> SearchTree -> [Solution]
limitedDfs _ (Sol sols)  = [sols]
limitedDfs 0 _           = []
limitedDfs n (Node _ st) = [ s | t <- st, s <- limitedDfs (n-1) t]
