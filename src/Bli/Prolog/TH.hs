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
import Bli.Prolog.Parser as P
import Text.ParserCombinators.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote

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
          case parse programP "" s of
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
          case parse bliCommandP "" ("?- "++s) of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]

