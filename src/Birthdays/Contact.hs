module Birthdays.Contact(
  Contact,
  Contact(..),
  nextBirthday,
  hasBirthdayInNDays,
  contactsThatHaveBirthdayInNDays
) where

import Control.Monad

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
    nextDate  day = getZonedDay >>= (\today -> if day < today then (nextDate $ addYear day) else return day)

hasBirthdayInNDays :: Integer -> Contact -> IO Bool
hasBirthdayInNDays n contact = (nextBirthday contact) >>= (isInNDays n)

contactsThatHaveBirthdayInNDays :: Integer -> [Contact] -> IO [Contact]
contactsThatHaveBirthdayInNDays n contacts = (filterM (hasBirthdayInNDays n) contacts)
