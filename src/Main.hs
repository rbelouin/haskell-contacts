import Control.Monad

import Data.List
import Data.Time.Calendar
import Data.Time.LocalTime

import Birthdays.Time

data Contact = Contact {
  id :: String,
  lastName :: String,
  firstName :: String,
  birthDate :: Day
} deriving Show

nextBirthday :: Contact -> IO Day
nextBirthday contact = nextDate (birthDate contact)
  where
    addYear   day = addGregorianYearsClip 1 day
    nextDate  day = getZonedDay >>= (\today -> if day < today then (nextDate $ addYear day) else return day)

hasBirthdayInNDays :: Integer -> Contact -> IO Bool
hasBirthdayInNDays n contact = (nextBirthday contact) >>= (isInNDays n)

contacts :: [Contact]
contacts = [Contact "1" "JAMAL" "Ahmad" (fromGregorian 1930 7 2), Contact "2" "ROMANO" "Aldo" (fromGregorian 1941 1 16), Contact "3" "BRUBECK" "Dave" (fromGregorian 1920 12 6)]

contactsThatHaveBirthdayInNDays :: Integer -> IO [Contact]
contactsThatHaveBirthdayInNDays n = (filterM (hasBirthdayInNDays n) contacts)

main :: IO ()
main = (contactsThatHaveBirthdayInNDays 8) >>= print
