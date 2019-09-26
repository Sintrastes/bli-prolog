{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.Prolog.TemplateHaskell(
  goal
) where

import Data.Prolog.Ast
import Prolog.Parser as P
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
          case parse bliCommandP "" s of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]



