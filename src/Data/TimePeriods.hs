
module Data.TimePeriods where

import Data.Interval
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock

data TimePeriod = Union [TimeInterval] deriving(Eq, Show)

type TimeInterval = Interval UTCTime

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