
-- | Datatypes used for representing the different kinds of 
--   commands that can be requested of bli prolog, and
--   the sorts of responses produced by those commands.
--
-- This API should be used by both the cli and our
-- REST API to encourage code re-use.
--

module Bli.App.Api where

import Bli.Prolog.Typechecking
import Bli.Prolog.Interp.Data

-- | A data type to model the types of 
--   requests that can be made to the server
data BliRequest = 
-- | A simple Get request to 
--   make a query and return the
--   results of that query.
     MakeQuery String
-- | Request that an assertion be made.
   | MakeAssertion String deriving(Eq, Show)

-- | A data type to model the types of responses
--   that the server can return to clients.
data BliResponse = 
  -- | Response to return when 
     SyntaxError InvalidClause
  -- | Response to successful query
   | QuerySuccess String
   | AssertionSuccess
   | AssertionFail_AlreadyAsserted deriving(Eq, Show)

data BliResult =
   Result_SyntaxError String
 | Result_QueryFail_BoundVarNotInBody
 | Result_QueryFail_AtomsNotInSchema [String]
-- ... Encountered type errors:
--         In predicate X, argument n is not of type Y, but rather of type W.
 | Result_QueryFail_TypeError [(String, Int, String, String)]
-- ... Identifier X is being used as an Nary predicate, but is declared to
--     be a term of type Y in the schema.
-- Note: This last argument should be optional here, if it has
-- not actually been declared as a term.
 | Result_QueryFail_TypeNotDeclared String
 | Result_QueryFail_EntityNotDeclared String String
 | Result_QueryFail_NotAPredicate [(String, Int, String)]
 | Result_QuerySuccess [Solution]
 | Result_AssertionSuccess
 | Result_AssertionFail_AlreadyAsserted
-- Error: X should have arity Y.
--        Z should have arity W.
--        ... etc...
 | Result_AssertionFail_AtomsNotInSchema [String]
 | Result_AssertionFail_NotAPredicate [(String, Int, String)]
 | Result_AssertionFail_TypeError [(String, Int, String, String)]
 | Result_AssertionFail_TypeNotDeclared String
 | Result_AssertionFail_CannotDeclareEntityOfBuiltinType String
 | Result_AssertionFail_CannotDeclaraDatatypeAsEntity
 | Result_AssertionFail_EntityNotDeclared String String


--data BliResult =
--   SyntaxError String
-- | QueryFail [InvalidClause]
-- | AssertionFail [InvalidClause]
-- | QuerySuccess [Soltuions]

{-
-- Note: We could probably refactor BliResult in an even better way
-- Below I'm just playing around with some ideas, at the moment:

data Query
data Assertion
data Fail
data Success

-- If we use a GADT such as the following, we can have different "subtypes"
-- for assertions and queries.

data BliResult' a b where
  Result_SyntaxError :: String ->                                        BliResult' a Fail
  Result_QueryFail_BoundVarNotInBody ::                                  BliResult' Query Fail
  Result_QueryFail_AtomsNotInSchma   :: [String]                      -> BliResult' Query Fail
  Result_QueryFail_TypeError :: [(String,Int,String,String)]          -> BliResult' Query Fail
  Result_QueryFail_NotAPredicate :: [(String, Int, Maybe String)]     -> BliResult' Query Fail
  Result_QuerySuccess     :: [Solution]                               -> BliResult' Query Success
  Result_AssertionSuccess ::                                             BliResult' Assertion Success
  Result_AssertionFail_AtomsNotInSchema :: [String]                   -> BliResult' Assertion Fail
  Result_AssertionFail_NotAPredicate :: [(String, Int, Maybe String)] -> BliResult' Assertion Fail
  Result_AssertionFail_TypeError :: [(String, Int, String, String)]   -> BliResult' Assertion Fail
-}