
--
-- | Parser for datetime literals and time periods
--

module Bli.Prolog.Parser.DateTime where

import Data.TimePeriods
import Bli.Prolog.Parser.Util
import Control.Monad (replicateM)
import Text.ParserCombinators.Parsec

dayP :: Parser InternalTime
dayP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: Parser Integer
  _ <- char '-'
  month' <- read <$> replicateM 2 digit :: Parser Int
  _ <- char '-'
  day' <- read <$> replicateM 2 digit :: Parser Int
  return $ InternalDay year' month' day'

monthP :: Parser InternalTime
monthP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: Parser Integer
  _ <- char '-'
  month' <- read <$> replicateM 2 digit :: Parser Int
  return $ InternalMonth year' month'

yearP :: Parser InternalTime
yearP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: Parser Integer
  return $ InternalYear year'

timePeriodP :: Parser TimePeriodInternal
timePeriodP = do
  periods <- sepBy1 (try dayP <|> try monthP <|> yearP) (symb "\\/")
  return $ InternalUnion periods
