{-# LANGUAGE DeriveLift #-}

module Data.TimePeriods where

import Data.Interval
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Language.Haskell.TH.Lift
import Data.Data
import GHC.Generics
import Data.Serialize
import Data.Time.Calendar
import Data.Time.Clock

instance Lift UTCTime

instance (Lift a, Ord a, Data a) => Lift (Interval a)
instance (Ord a) => Ord (Interval a)

type TimeInterval = Interval UTCTime
data TimePeriod = Union [TimeInterval] deriving(Eq, Show, Read, Lift, Ord)

data InternalTime = 
    InternalDay   Integer Int Int
  | InternalMonth Integer Int
  | InternalYear  Integer deriving(Eq, Show, Read, Lift, Data, Ord, Generic)

instance Serialize InternalTime

type TimeIntervalInternal = Interval InternalTime
data TimePeriodInternal = InternalUnion [InternalTime] deriving(Eq, Show, Read, Lift, Ord, Generic)

instance Serialize TimePeriodInternal

-- Internal representation of datetime literals to use instead of the
-- UTCTime based type to allow for binary serialization.
fromInternal :: TimePeriodInternal -> TimePeriod
fromInternal (InternalUnion internalTimeIntervals) = Union $ map help internalTimeIntervals
  where help (InternalDay y m d) = day y m d
        help (InternalMonth y m) = month y m
        help (InternalYear y) = year y

--instance Generic DiffTime where
--  to difftime = to $ from (diffTimeToPicoseconds difftime)
--  from typeRepOfInt = picoSecondsToDiffTime $ from $ to typeRepOfInt
-- instance Serialize DiffTime
--
--deriving instance Generic Day
--instance Serialize Day
--
--deriving instance Generic UTCTime
--instance Serialize UTCTime

day :: Integer -> Int -> Int -> TimeInterval
day year month day = 
  case fromGregorianValid year month (day + 1) of
    Just time -> (Finite $ UTCTime (fromGregorian year month day) 0) <=..< (Finite $ UTCTime time 0)
    Nothing ->
      case fromGregorianValid year (month + 1) 1 of
        Just time -> (Finite $ UTCTime (fromGregorian year month day) 0) <=..< (Finite $ UTCTime time 0)
        Nothing -> 
          (Finite $ UTCTime (fromGregorian year month day) 0) <=..< (Finite $ UTCTime (fromGregorian (year + 1) 1 1) 0)
          
month :: Integer -> Int -> TimeInterval
month year month = 
  case fromGregorianValid year (month + 1) 1 of
    Just time -> (Finite $ UTCTime (fromGregorian year month 1) 0) <=..< (Finite $ UTCTime time 0)
    Nothing   -> (Finite $ UTCTime (fromGregorian year month 1) 0) <=..< (Finite $ UTCTime (fromGregorian (year + 1) 1 1) 0) 

year :: Integer -> TimeInterval
year year = (Finite $ UTCTime (fromGregorian year 1 1) 0) <=..< (Finite $ UTCTime (fromGregorian (year + 1) 1 1) 0)