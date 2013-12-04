module Birthdays.Time(
  getZonedDay,
  isInNDays
) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.LocalTime

getZonedDay :: IO Day
getZonedDay = fmap (localDay . zonedTimeToLocalTime) getZonedTime 

isInNDays :: Integer -> Day -> IO Bool
isInNDays n day = fmap (elem day) nDays
  where nDays = fmap (\today -> [today..(addDays n today)]) getZonedDay
