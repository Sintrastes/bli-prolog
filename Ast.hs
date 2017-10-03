module Ast where

----------------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------------

type Goal = [Term]
type Program = Clauses
type Clauses = [Clause]
type Clause = (Term, Terms) -- head and body
data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Show, Read)
type Terms = [Term]
type Atom = String
type Variable = String
