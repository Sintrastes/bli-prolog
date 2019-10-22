{-# LANGUAGE DeriveLift #-}

--
-- | Basic datatypes for the AST
--   of bedelibry prolog terms.
--

module Data.Bli.Prolog.Ast where

import Language.Haskell.TH.Lift
import Data.List (intercalate)
import Data.Bli.Prolog.Schema
import Data.TimePeriods

-- | An internal representation of prolog terms.
data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Ord, Read, Lift) 

instance Show Term where
  show (Var x) = x
  show (Comp id []) = show id
  show (Comp id ts) = show id ++ "(" ++ (intercalate "," (map show ts)) ++ ")" 

-- | Used for identifiers for entities and prolog relations/predicates.
--   Must begin with a lowercase letter.
data Atom =
    Identifier String
  | Predicate String Terms
  | DataLit String Atoms
  | AppTerm String Atoms
  | IntLiteral Int
  | ListLiteral [Atom]
  | StringLiteral String
  | Rule Term Terms
  | Goal Terms
  | TimeperiodLiteral TimePeriod deriving(Eq, Ord, Read, Lift)

instance Show Atom where
  show (Identifier x) = x
  show (DataLit x []) = "'" ++ x
  show (DataLit x xs) = "'" ++ x ++ "(" ++ intercalate ", " (map show xs) ++ ")"
  show (Predicate x ts) = x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (AppTerm x ts) = x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (IntLiteral n) = show n
  show (ListLiteral xs) = show xs
  show (StringLiteral str) = "\"" ++ str ++ "\""
  show (Rule t ts) = "{" ++ show t ++ " :- " ++ intercalate ", " (map show ts) ++ "}"
  show (Goal ts) = intercalate ", " $ map show ts
  show (TimeperiodLiteral timeperiod) = show timeperiod

type Atoms = [Atom]

-- | Must begin with an uppercase letter.
type Variable = String

type Terms = [Term]
-- | A goal is just a list of terms.
type Goal = [Term]

-- | A goal bound within a lambda abstraction.
type LambdaGoal = ([Variable],[Term])

-- | A prolog clause, representing a rule. i.e. [HEAD] :- [BODY].
type Clause = (Term, Terms)

prettyShowClause (head, []) = show head ++ "."
prettyShowClause (head, body) = (show head) ++ " :-\n  " ++ intercalate ", " (map show body) ++ "."   

-- | A data type for bli prolog commands, which can either
--   be a query, or an assertion. Note that here,
--   a plain query/assertion such as 
--
--     person(nate). 
--   
--   Is sometimes interpreted in the source code as a degenerate clause, i.e.
--   
--     person(nate) :- .

-- | Version of BliCommand, using our typed schemas.
data BliCommand =
   QueryMode Goal
 | LambdaQuery LambdaGoal
 | MkAlias String String 
 | AssertMode Goal
 | AssertClause Clause 
 | AssertSchema SchemaEntry deriving(Show, Eq, Lift)

-- | Helper function to check if a BliCommand is an assertion.
isAssertion :: BliCommand -> Bool
isAssertion (QueryMode _) = False
isAssertion (LambdaQuery _) = False
isAssertion _ = True

-- | Helper function to check if a BliCommand is a query.
isQuery :: BliCommand -> Bool
isQuery = not . isAssertion

type Program = Clauses
type Clauses = [Clause]

-- | A type synonym for BliPrograms,
-- which can contain both assertions and queries.
-- (Note: this may or may not be needed).
type BliProgram = [BliCommand]
