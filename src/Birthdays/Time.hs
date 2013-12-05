module Birthdays.Time(
  Day,
  addYear,
  fromGregorian,
  getZonedDay,
  isInNDays
) where

import Control.Monad

import qualified Data.Time.Calendar

import Data.Time.Calendar (addDays, addGregorianYearsClip)
import Data.Time.LocalTime (localDay, zonedTimeToLocalTime, getZonedTime)

type Day = Data.Time.Calendar.Day

addYear :: Day -> Day
addYear = addGregorianYearsClip 1

fromGregorian :: Integer -> Int -> Int -> Day
fromGregorian = Data.Time.Calendar.fromGregorian

getZonedDay :: IO Day
getZonedDay = fmap (localDay . zonedTimeToLocalTime) getZonedTime 

isInNDays :: Integer -> Day -> IO Bool
isInNDays n day = fmap (elem day) nDays
  where nDays = fmap (\today -> [today..(addDays n today)]) getZonedDay
