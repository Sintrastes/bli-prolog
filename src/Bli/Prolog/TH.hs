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
import Bli.App.Config (AppConfig)
import Text.ParserCombinators.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Bli.Prolog (runtimeCfg) -- ^ This probably won't cause dependency issues, but there might be a better place to put this.
import Control.Monad.Bli.Pure
import Data.BliParser hiding (bli)

-- Note: These need to take in some sort of language configuration now.

-- | The default options to use for our quasiquoters.
defaultOpts :: AppConfig
defaultOpts = runtimeCfg

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
          case initBli defaultOpts (parseBli goalP s) of
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
          case initBli defaultOpts (parseBli prologProgramP s) of
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
          case initBli defaultOpts (parseBli bliCommandTypedP s) of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]

