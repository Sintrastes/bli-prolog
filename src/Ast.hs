module Ast where

----------------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------------

import Language.Haskell.TH.Lift

type Goal = [Term]
type Program = Clauses
type Clauses = [Clause]
type Clause = (Term, Terms) -- head and body
data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Show, Read, Lift)
type Terms = [Term]
type Atom = String
type Variable = String
