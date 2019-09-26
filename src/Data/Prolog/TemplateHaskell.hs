{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.Prolog.TemplateHaskell(
  goal
) where

import Data.Prolog.AST
import Prolog.Parser as P
import Language.Haskell.TH
import Language.Haskell.TH.Quote

goal :: QuasiQuoter
goal = QuasiQuoter {
    quoteExp  = undefined
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."



