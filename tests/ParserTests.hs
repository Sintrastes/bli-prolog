
-- | Entrypoint for the test suite of the Bli Prolog
--   parser.

module ParserTests where

import Test.Tasty
import Test.Tasty.HUnit

import Prolog.Parser
import Tests.Parsers

main = defaultMain $ testGroup "bli prolog parser test suite."
  [parser_tests]
