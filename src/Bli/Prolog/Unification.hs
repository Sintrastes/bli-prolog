
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
import Control.Monad

-- | A unifier is a list of pairs of variables, and their corresponding terms, which
--   which when we substitute into a term, replaces all occurences of the free variables
--   in the list with the corresponding variable.
type Unifier t = [(Variable, t)]

class Unifiable t where
  variables :: t -> [Variable]
  subs      :: Unifier t -> t -> t
  unify     :: t -> t -> (Maybe (Unifier t))

-- | 
compose :: Unifiable t => Unifier t -> Unifier t -> Unifier t
compose u1 u2 = (map (\(v, t) -> (v, subs u2 t)) u1) ++ u2

