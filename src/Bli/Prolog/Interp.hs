
module Bli.Prolog.Interp where

import Data.List (nub)
import Control.Monad(liftM)
import Control.Monad.Bli.Pure
import Data.Foldable

import Data.Bli.Prolog.Ast
import Bli.Prolog.Interp.Data

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


-- | Helper function to get just the parts of the goal that
--   consist of *type predicates* -- i.e. predicates of the form
--   id(X), where id is the identifier for a type in the current schema.
getTypePredicates :: Goal -> Bli Goal
getTypePredicates goal = return goal

-- | Helper function to return a list of all of the stored
--   relations (along with their type identifiers) which
--   are used in a goal.
--
--   Used in solve so that we can apply the corrent logic
--   for relations that are stored in bedelibry.
getStoredRelations :: Goal -> Bli (String, [String])
getStoredRelations goal = undefined

-- Note: Here is where we can put our custom logic for
-- e.x. dealing with the bedelibry backend.
solve :: Goal -> Bli [SearchTree]
solve g = do
  prog <- toList <$> getFacts
  typePredicates <- getTypePredicates g
  case () of
   _ | typePredicates /= [] -> return $ solve' prog g
     | otherwise -> return $ solve' prog g

-- Uses the List monad for backtracking
solve' :: Program -> Goal -> [SearchTree]
solve' _ [r] | isReportGoal r = [Sol $ getSolution r]
solve' prog g@(t1 : ts) = [Node g trees]
    where trees = do c <- prog
                     let (tc, tsc) = freshen (variables g) c
                     case unify tc t1 of
                       Just u -> do
                         let g' = map (subs u) $ tsc ++ ts
                         solve' prog g'
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
makeReportTree :: Goal -> Bli SearchTree
makeReportTree goal = do 
  branches <- solve (goal ++ makeReportGoal goal)
  return $ Node goal branches 
