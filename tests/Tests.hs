
--
-- Entrypoint for testsuite
--

import Test.Tasty
import Test.Tasty.HUnit

import Prolog.Parser
import Prolog.Interp
import Data.Prolog.Ast

import Tests.Old.Siblings

main = defaultMain siblings_test

