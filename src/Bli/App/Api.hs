
-- | Datatypes used for representing the different kinds of 
--   commands that can be requested of bli prolog, and
--   the sorts of responses produced by those commands.
--
-- This API should be used by both the cli and our
-- REST API to encourage code re-use.
--

module Bli.App.Api where

import Prolog.Analysis
import Prolog.Interp

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
 | Result_QueryFail_NotAPredicate [(String, Int, Maybe String)]
 | Result_QuerySuccess [Solution]
 | Result_AssertionSuccess
 | Result_AssertionFail_AlreadyAsserted
-- Error: X should have arity Y.
--        Z should have arity W.
--        ... etc...
 | Result_AssertionFail_AtomsNotInSchema [String]
 | Result_AssertionFail_NotAPredicate [(String, Int, Maybe String)]
 | Result_AssertionFail_TypeError [(String, Int, String, String)]