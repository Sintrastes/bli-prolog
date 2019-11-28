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
import Data.Serialize
import GHC.Generics

-- | An internal representation of prolog terms.
data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Ord, Read, Lift, Generic)

instance Serialize Term 

instance Show Term where
  show (Var x) = x
  show (Comp id []) = show id
  show (Comp id ts) = show id ++ "(" ++ (intercalate "," (map show ts)) ++ ")" 

-- | Used for identifiers for entities and prolog relations/predicates.
--   Must begin with a lowercase letter.
data Atom =
    AtomVar Variable
  | Identifier String
  | Predicate String Terms
  | DataLit String Atoms
  | AppTerm String Atoms
  | IntLiteral Int
  | FloatLiteral Double
  | ListLiteral [Atom]
  | StringLiteral String
  | Rule Term Terms
  | Goal Terms
  | TimeperiodLiteral TimePeriodInternal deriving(Eq, Ord, Read, Lift, Generic)

instance Serialize Atom

instance Show Atom where
  show (AtomVar x) = x
  show (Identifier x) = x
  show (DataLit x []) = "'" ++ x
  show (DataLit x xs) = "'" ++ x ++ "(" ++ intercalate ", " (map show xs) ++ ")"
  show (Predicate x ts) = x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (AppTerm x ts) = x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (IntLiteral n) = show n
  show (FloatLiteral n) = show n
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

-- | A lambda goal is just a list of terms, and a list of free variables that occur in the goal, which may or may not 
--   be explicitly bound by a lambda abstraction.
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
data BliCommand =
   Query LambdaGoal
-- | LambdaQuery LambdaGoal
 | MkAlias String String 
 | Assert Goal
 | AssertClause Clause 
 | AssertSchema SchemaEntry deriving(Show, Eq, Lift, Generic)

instance Serialize BliCommand

-- | Helper function to check if a BliCommand is an assertion.
isAssertion :: BliCommand -> Bool
isAssertion (Query _) = False
isAssertion (MkAlias _ _) = False
isAssertion _ = True

-- | Helper function to check if a BliCommand is a query.
isQuery :: BliCommand -> Bool
isQuery (Query _) = True
isQuery _ = False 

type Program = Clauses
type Clauses = [Clause]

data BliProgram = 
    Program [BliCommand]
  | Module String [BliCommand]
