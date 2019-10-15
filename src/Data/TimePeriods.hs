{-# LANGUAGE DeriveLift #-}

module Data.TimePeriods where

import Data.Interval
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Language.Haskell.TH.Lift
import Data.Data

instance Lift UTCTime
instance (Lift a, Ord a, Data a) => Lift (Interval a)
instance (Ord a) => Ord (Interval a)

data TimePeriod = Union [TimeInterval] deriving(Eq, Show, Lift, Ord)

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