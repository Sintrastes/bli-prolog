
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
   Result_QueryFail InvalidClause
-- Note: The above should probably be refactored into something like:
--   Result_SyntaxError
--   Result_QueryFail_BoundVarNotInBody
--   Result_QueryFail_AtomsNotInSchema [String]
--   Result_QueryFail_WrongArities [(String,Int)]
-- A syntax error (which can arrise from an InvalidClause)
-- is not associated with either a query, or an assertion,
-- and so the structure of out ADT should reflect this.    
 | Result_QueryFail_WrongArities [(String,Int)]
-- ... Encountered type errors:
--         In predicate X, argument n is not of type Y, but rather of type W.
 | Result_QueryFail_TypeError [(String, Int, String, String)]
-- ... Identifier X is being used as an Nary predicate, but is declared to
--     be a term of type Y in the schema.
 | Result_QueryFail_NotAPredicate [(String, Int, String)]
 | Result_QuerySuccess [Solution]
 | Result_AssertionSuccess
 | Result_AssertionFail [String]
 | Result_AssertionFail_AlreadyAsserted
-- Error: X should have arity Y.
--        Z should have arity W.
--        ... etc...
 | Result_AssertionFail_WrongArities [(String,Int)] deriving(Eq, Show)