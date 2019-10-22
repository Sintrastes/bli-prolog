
module Bli.Prolog.Unification.Terms where

import Data.Bli.Prolog.Ast
import Bli.Prolog.Unification
import Data.List
import Control.Monad

-- | Helper function to check if a variable occurs in a term.
occursIn :: Variable -> Term -> Bool
occursIn v (Var x)     = v == x
occursIn v (Comp _ ms) = any (occursIn v) ms

-- | Substitutes all free variables in the given
--   term with the matching terms in the unifier.
subsTerm :: (Unifier Term) -> Term -> Term
subsTerm u t@(Var x)   = maybe t id (lookup x u)
subsTerm u (Comp n ts) = Comp n (map (subs u) ts)

-- | Tries to unify two terms.
unifyTerm :: Term -> Term -> Maybe (Unifier Term)
-- "_" matches anything
unifyTerm (Var "_") t                      = return []
unifyTerm t (Var "_")                      = return []
unifyTerm (Var x) (Var y) | x == y         = return []
unifyTerm (Var x) t | not(x `occursIn` t)  = return [(x, t)]
unifyTerm t v@(Var _)                      = unify v t
unifyTerm (Comp m ms) (Comp n ns) | m == n = unifyListTerms ms ns
unifyTerm _ _                              = Nothing

-- | Helper function which tries to unify a list of 
--   terms simultaneously.
unifyListTerms :: [Term] -> [Term] -> Maybe (Unifier Term)
unifyListTerms (t : ts) (r : rs) =
    do u1 <- unifyTerm t r
       u2 <- unifyListTerms (map (subs u1) ts) (map (subs u1) rs)
       return $ u1 `compose` u2
unifyListTerms [] [] = Just []
unifyListTerms _ _   = Nothing

-- | Helper function which returns the list of free variables 
--   in a term.

variablesTerms :: Terms -> [Variable]
variablesTerms ts = nub $ varsList ts
    where vars (Var "_") = []
          vars (Var x) = [x]
          vars (Comp _ ts) = varsList ts
          varsList ts = [ v | t <- ts, v <- vars t]


-- | Helper function... (freshens variables in a clause?)
freshen :: (Foldable f) => f Variable -> (Term, [Term]) -> (Term, [Term])
freshen bound (tc, tb) = (subs sub tc, map (subs sub) tb)
    where vars = join $ map variables (tc : tb)
          sub = [ (v, Var $ nextVar 0 v) | v <- vars, v `elem` bound]
          nextVar i v = let v' = "_" ++ show i ++ "_" ++ v in
                        if v' `elem` bound then nextVar (i+1) v
                        else v'

instance Unifiable Term where
  variables t = variablesTerms [t]
  subs = subsTerm
  unify = unifyTerm  
