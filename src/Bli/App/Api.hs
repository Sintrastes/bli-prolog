
-- | Datatypes used for representing the different kinds of 
--   commands that can be requested of bli prolog, and
--   the sorts of responses produced by those commands.
--
-- This API should be used by both the cli and our
-- REST API to encourage code re-use.
--

module Bli.App.Api where

import Prolog.Analysis

-- | A data type to model the types of 
--   requests that can be made to the server
data BliRequest = 
-- | A simple Get request to 
--   make a query and return the
--   results of that query.
     MakeQuery String
-- | Request that an assertion be made.
   | MakeAssertion String

-- | A data type to model the types of responses
--   that the server can return to clients.
data BliResponse = 
  -- | Response to return when 
     SyntaxError InvalidClause
  -- | Response to successful query
   | QuerySuccess String
   | AssertionSuccess
