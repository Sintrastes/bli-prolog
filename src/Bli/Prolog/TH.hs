{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

--
-- | Some Template Haskell quasiquoters -- useful for running tests, and for
--   using bedelibry prolog as an EDSL within Haskell.
--

module Bli.Prolog.TH (
    goal,
    bli,
    program
) where

import Data.Bli.Prolog.Ast
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser
import Bli.Prolog.Parser.Terms
import Bli.Prolog.Parser.Cli
import Bli.Prolog.Parser.Schema
import Text.ParserCombinators.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Note: These need to take in some sort of language configuration now.

goal :: QuasiQuoter
goal = QuasiQuoter {
    quoteExp  = parserTH
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."
        parserTH :: String -> Q Exp
        parserTH s =
          case parse goalP "" s of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]

program :: QuasiQuoter
program = QuasiQuoter {
    quoteExp  = parserTH
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."
        parserTH :: String -> Q Exp
        parserTH s =
          case parse prologProgramP "" s of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]


bli :: QuasiQuoter
bli = QuasiQuoter {
    quoteExp  = parserTH
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."
        parserTH :: String -> Q Exp
        parserTH s =
          case parse bliCommandTypedP "" s of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]

