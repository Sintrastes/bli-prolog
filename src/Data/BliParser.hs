
module Data.BliParser where

import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec (parse)
import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Bli.Trans.Generic
import Control.Applicative
import Control.Monad.Bli.Common
import Control.Monad.Bli
import Control.Monad.Trans.State
import Bli.App.Api
import Data.Either.Combinators (mapLeft)

type GenericBliParser t1 t2 t3 t4 alias m a = ParsecT String () (StateT (BliStore t1 t2 t3 t4 alias) m) a

type BliParser a = GenericBliParser FactContainer RelationContainer EntityContainer TypeContainer AliasDatastructure IO a

bli = lift

-- Note: his is a helper function that should be defined elsewhere
joinResults :: [BliResult] -> BliResult
joinResults = undefined

-- | Parses a BliParser.
parseBli :: BliParser a -> String -> Bli (Either BliResult a)
parseBli parser string = (mapLeft handleParseError) 
                     <$> (runParserT parser () "" string)

-- | Helper function to convert parser errors into the appropriate BliResult format.
handleParseError :: ParseError -> BliResult
handleParseError parseError = undefined
  -- If the message can be parsed as a BliResult,
  -- wrap it as a standard BliResult, otherwise,
  -- wrap it as a parser error.
  -- Note: I think the basic use case here is for
  -- FeatureNotEnabled errors -- otherwise,
  -- a more principled approach to error handling should
  -- probably be taken here.
  where msgs = errorMessages parseError
        handleMessage (Message msg) = undefined
        handleMessage msg = Result_SyntaxError (messageString msg)


handleParseErrors :: [ParseError] -> [BliResult]
handleParseErrors = map handleParseError