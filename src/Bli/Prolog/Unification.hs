
--
-- | Code for unification. Eventually I want to make this work
--   for both terms and type variables.
--
--   This could also be generic. Consider using Data.Map for
--   unifiers.
--

module Bli.Prolog.Unification where

import Data.Bli.Prolog.Ast
import Data.List

-- | A unifier is a list of pairs of variables, and their corresponding terms, which
--   which when we substitute into a term, replaces all occurences of the free variables
--   in the list with the corresponding variable.
type Unifier = [(Variable, Term)]

-- | 
compose :: Unifier -> Unifier -> Unifier
compose u1 u2 = (map (\(v, t) -> (v, subs u2 t)) u1) ++ u2

-- | Helper function to check if a variable occurs in a term.
occursIn :: Variable -> Term -> Bool
occursIn v (Var x)     = v == x
occursIn v (Comp _ ms) = any (occursIn v) ms

-- | Substitutes all free variables in the given
--   term with the matching terms in the unifier.
subs :: Unifier -> Term -> Term
subs u t@(Var x)   = maybe t id (lookup x u)
subs u (Comp n ts) = Comp n (map (subs u) ts)

-- | Tries to unify two terms.
unify :: Term -> Term -> Maybe Unifier
-- "_" matches anything
unify (Var "_") t                      = return []
unify t (Var "_")                      = return []
unify (Var x) (Var y) | x == y         = return []
unify (Var x) t | not(x `occursIn` t)  = return [(x, t)]
unify t v@(Var _)                      = unify v t
unify (Comp m ms) (Comp n ns) | m == n = unifyList ms ns
unify _ _                              = Nothing

-- | Helper function which tries to unify a list of 
--   terms simultaneously.
unifyList :: [Term] -> [Term] -> Maybe Unifier
unifyList (t : ts) (r : rs) =
    do u1 <- unify t r
       u2 <- unifyList (map (subs u1) ts) (map (subs u1) rs)
       return $ u1 `compose` u2
unifyList [] [] = Just []
unifyList _ _   = Nothing

-- | Helper function which returns the list of free variables 
--   in a term.
variables :: Terms -> [Variable]
variables ts = nub $ varsList ts
    where vars (Var "_") = []
          vars (Var x) = [x]
          vars (Comp _ ts) = varsList ts
          varsList ts = [ v | t <- ts, v <- vars t]

-- | Helper function... (freshens variables in a clause?)
freshen :: Foldable t => t Variable -> (Term, Terms) -> (Term, Terms)
freshen bound (tc, tb) = (subs sub tc, map (subs sub) tb)
    where vars = variables (tc : tb)
          sub = [ (v, Var $ nextVar 0 v) | v <- vars, v `elem` bound]
          nextVar i v = let v' = "_" ++ show i ++ "_" ++ v in
                        if v' `elem` bound then nextVar (i+1) v
                        else v'
