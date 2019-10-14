{-# LANGUAGE DeriveLift #-}

--
-- | Basic datatypes for the AST
--   of bedelibry prolog terms.
--

module Data.Bli.Prolog.Ast where

import Language.Haskell.TH.Lift
import Data.List (intercalate)
import Data.Bli.Prolog.Schema

-- | An internal representation of prolog terms.
data Term = Var Variable
          | Comp Atom Terms
          deriving (Eq, Read, Ord, Lift)


data BliPrologType = 
   Entity 
 -- A user declared type, such as "person".
 | DeclaredType String
 -- Type of types, "type".
 | TypTypes
 | Predicate [BliPrologType]
-- Note: Is there really a difference between predicates and goals?
 | Goal [BliPrologType]
 -- A type for rules: This allows for an interesting
 -- design choice, where we allow for "first-class rules.",
 -- and so predicates are allowed to talk about rules.
 | Rule 
 | StringLit
 | IntLit
 | DateTimeLit
 -- A polymorphic list datatype
 | List BliPrologType
 | DateLit deriving(Eq)

instance Show BliPrologType where
  show Entity = "entity"
  show (DeclaredType str) = str
  show TypTypes = "type"
  show (Predicate types) = "rel[" ++ (intercalate ", " (map show types)) ++ "]"
  show (Goal types) = "goal[" ++ (intercalate ", " (map show types)) ++ "]"
  show Rule = "rule"
  show StringLit = "string"
  show IntLit = "int"
  show DateTimeLit = "datetime"
  show (List t) = "list["++ show t ++"]" 
  show DateLit = "date"

-- Subtyping relation.
infixr 9 <:
(<:) :: BliPrologType -> BliPrologType -> Bool
(<:) _ TypTypes = True
(<:) _ _ = undefined

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
data BliCommand = QueryMode Goal 
                | AssertMode Goal
                | AssertClause Clause
             -- Make new assertions into the active schema
                | AssertTypePred SchemaEntry
                | LambdaQuery LambdaGoal
  deriving(Show, Eq, Lift)

-- | Version of BliCommand, using our typed schemas.
data BliCommandTyped =
   T_QueryMode Goal
 | T_LambdaQuery LambdaGoal
 | T_AssertMode Goal
 | T_AssertClause Clause 
 | T_AssertSchema TypedSchemaEntry deriving(Show, Eq, Lift)

-- | Helper function to check if a BliCommand is an assertion.
isAssertion :: BliCommandTyped -> Bool
isAssertion (T_QueryMode _) = False
isAssertion (T_LambdaQuery _) = False
isAssertion _ = True

-- | Helper function to check if a BliCommand is a query.
isQuery :: BliCommandTyped -> Bool
isQuery = not . isAssertion

type Program = Clauses
type Clauses = [Clause]

-- | A type synonym for BliPrograms,
-- which can contain both assertions and queries.
-- (Note: this may or may not be needed).
type BliProgram = [BliCommand]
