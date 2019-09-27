{-# LANGUAGE DeriveLift #-}

--
-- | Basic datatypes for the AST
--   of bedelibry prolog terms.
--

module Data.Prolog.Ast where

import Language.Haskell.TH.Lift
import Data.List (intercalate)

-- | An internal representation of prolog terms.
data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Read, Lift)

instance Show Term where
  show (Var x) = x
  show (Comp id []) = id
  show (Comp id ts) = id ++ "(" ++ (intercalate "," (map show ts)) ++ ")" 

-- | Used for identifiers for entities and prolog relations/predicates.
--   Must begin with a lowercase letter.
type Atom = String

-- | Must begin with an uppercase letter.
type Variable = String

type Terms = [Term]
-- | A goal is just a list of terms.
type Goal = [Term]

-- | A goal bound within a lambda abstraction.
type LambdaGoal = ([Variable],[Term])

-- | A prolog clause, representing a rule. i.e. [HEAD] :- [BODY].
type Clause = (Term, Terms)

-- | A data type for bli prolog commands, which can either
--   be a query, or an assertion. Note that here,
--   a plain query/assertion such as 
--
--     person(nate). 
--   
--   Is sometimes interpreted in the source code as a degenerate clause, i.e.
--   
--     person(nate) :- .
data BliCommand = QueryMode Goal 
                | AssertMode Goal
                | AssertClause Clause
                | LambdaQuery LambdaGoal
  deriving(Show, Lift)

type Program = Clauses
type Clauses = [Clause]

-- | A type synonym for BliPrograms,
-- which can contain both assertions and queries.
-- (Note: this may or may not be needed).
type BliProgram = [BliCommand]
