
--
-- | Parser for datetime literals and time periods
--

module Bli.Prolog.Parser.DateTime where

import Data.TimePeriods
import Bli.Prolog.Parser.Util
import Control.Monad (replicateM)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Data.BliParser
import Bli.App.Config
import Bli.App.Config.Features

dayP :: BliParser InternalTime
dayP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: BliParser Integer
  _ <- char '-'
  month' <- read <$> replicateM 2 digit :: BliParser Int
  _ <- char '-'
  day' <- read <$> replicateM 2 digit :: BliParser Int
  return $ InternalDay year' month' day'

monthP :: BliParser InternalTime
monthP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: BliParser Integer
  _ <- char '-'
  month' <- read <$> replicateM 2 digit :: BliParser Int
  return $ InternalMonth year' month'

yearP :: BliParser InternalTime
yearP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: BliParser Integer
  return $ InternalYear year'

timePeriodP :: BliParser TimePeriodInternal
timePeriodP = do
  periods <- sepBy1 (try dayP <|> try monthP <|> yearP) ( try (ifEnabledP UnicodeSyntax (symb "∨")) <|> (symb "\\/"))
  return $ InternalUnion periods
