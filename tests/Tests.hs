
--
-- Entrypoint for testsuite
--

import Test.Tasty
import Test.Tasty.HUnit

import Prolog.Parser
import Prolog.Interp
import Data.Prolog.Ast

import Tests.Old.Siblings
import Tests.Parsers
import Tests.App
import Tests.Procs
import Tests.Literals
import Tests.Aliases
import Tests.Datatypes
import Tests.Typing

main = defaultMain $ testGroup "Bli prolog main test suite." $
     [siblings_test,
      parser_tests,
      app_tests,
      alias_tests,
      literal_tests,
      proc_tests,
      datatype_tests,
      typing_tests]
