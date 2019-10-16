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

data BliPrologType = 
   EntityT 
 -- Function from one BliPrologType to another.
 | FuncT BliPrologType BliPrologType
 -- A user declared type, such as "person".
 | DeclaredTypeT String
 -- Type of types, "type".
 | TypTypesT
 | PredicateT [BliPrologType]
 -- A type for rules: This allows for an interesting
 -- design choice, where we allow for "first-class rules.",
 -- and so predicates are allowed to talk about rules.
 | RuleT 
 | StringLitT
 | IntLitT
 | DateTimeLitT
 -- A polymorphic list datatype
 | ListT BliPrologType
 | DateLitT deriving(Eq)

instance Show BliPrologType where
  show EntityT = "entity"
  show (DeclaredTypeT str) = str
  show TypTypesT = "type"
  show (PredicateT []) = "pred"
  show (PredicateT types) = "pred[" ++ (intercalate ", " (map show types)) ++ "]"
  show RuleT = "rule"
  show StringLitT = "string"
  show IntLitT = "int"
  show DateTimeLitT = "datetime"
  show (ListT t) = "list["++ show t ++"]" 
  show DateLitT = "date"

-- Subtyping relation.
infixr 9 <:
(<:) :: BliPrologType -> BliPrologType -> Bool
(<:) _ TypTypesT = True
(<:) _ _ = undefined

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
  show (Predicate x ts) = x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (AppTerm x ts) = x ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (IntLiteral n) = show n
  show (ListLiteral xs) = show xs
  show (StringLiteral str) = "\"" ++ str ++ "\""
  show (Rule t ts) = show t ++ " :- " ++ intercalate ", " (map show ts) ++ "."
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
