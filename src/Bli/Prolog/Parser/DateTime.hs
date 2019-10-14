
--
-- | Parser for datetime literals and time periods
--

module Bli.Prolog.Parser.DateTime where

import Data.TimePeriods
import Bli.Prolog.Parser.Common
import Control.Monad (replicateM)
import Text.ParserCombinators.Parsec

dayP :: Parser TimeInterval
dayP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: Parser Integer
  _ <- char '-'
  month' <- read <$> replicateM 2 digit :: Parser Int
  _ <- char '-'
  day' <- read <$> replicateM 2 digit :: Parser Int
  return $ day year' month' day'

monthP :: Parser TimeInterval
monthP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: Parser Integer
  _ <- char '-'
  month' <- read <$> replicateM 2 digit :: Parser Int
  return $ month year' month'

yearP :: Parser TimeInterval
yearP = do
  _ <- char '\''
  year' <- read <$> replicateM 4 digit :: Parser Integer
  return $ year year'

timePeriodP :: Parser TimePeriod
timePeriodP = do
  periods <- sepBy1 (try dayP <|> try monthP <|> yearP) (symb "\\/")
  return $ Union periods
