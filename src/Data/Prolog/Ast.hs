{-# LANGUAGE DeriveLift #-}

--
-- Basic datatypes for the AST
-- of our prolog terms
--

module Data.Prolog.Ast where

----------------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------------

import Language.Haskell.TH.Lift

data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Show, Read, Lift)

type Terms = [Term]
type Atom = String
type Variable = String

type Goal = [Term]
-- A goal bound within a lambda abstraction.
type LambdaGoal = ([Variable],[Term])

type Clause = (Term, Terms) -- head and body

-- A data type for bli prolog commands, which can either
-- be a query, or an assertion. Note that here,
-- a plain query/assertion such as 
--
--   person(nate). 
--
-- Is interpreted as a degenerate clause, i.e.
--
--   person(nate) :- .
--

data BliCommand = QueryMode Goal 
                | AssertMode Goal
                | AssertClause Clause
                | LambdaQuery LambdaGoal
  deriving(Show, Lift)

type Program = Clauses
type Clauses = [Clause]

-- A type synonym for BliPrograms,
-- which can contain both assertions and queries.
-- (This may or may not be needed).
type BliProgram = [BliCommand]
