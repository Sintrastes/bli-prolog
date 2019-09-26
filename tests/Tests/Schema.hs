
--
-- A module for test involving bli schemas
--

module Tests.Schema where

-- Note: These imports may not be nescesary depending on how I structure
-- my unit tests.
import Test.Tasty
import Test.Tasty.HUnit

import Prolog.Parser
import Prolog.Interp
import Prolog.Ast

-- Test schema
schema :: Schema
schema = [("programming_language",1), ("name", 2)]
