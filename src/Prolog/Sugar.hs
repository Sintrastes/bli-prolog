
--
-- Utility functions for implementing some of the snytatic sugar 
-- of bli prolog.
--

module Prolog.Sugar

import Data.Schema
import Prolog.Parser
import Prolog.Interp
import Prolog.Ast

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
