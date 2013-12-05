import Control.Monad

import Data.ByteString.Char8
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime

import Birthdays.Time

import Database.HDBC
import Database.HDBC.PostgreSQL

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

staticContacts :: [Contact]
staticContacts = [Contact "1" "JAMAL" "Ahmad" (fromGregorian 1930 7 2), Contact "2" "ROMANO" "Aldo" (fromGregorian 1941 1 16), Contact "3" "BRUBECK" "Dave" (fromGregorian 1920 12 6)]

contactsThatHaveBirthdayInNDays :: Integer -> [Contact] -> IO [Contact]
contactsThatHaveBirthdayInNDays n contacts = (filterM (hasBirthdayInNDays n) contacts)

fromPgcontact :: [SqlValue] -> Maybe Contact
fromPgcontact [SqlByteString id, SqlByteString lastName, SqlByteString firstName, SqlLocalDate birthDate] = Just $ Contact (unpack id) (unpack lastName) (unpack firstName) birthDate
fromPgcontact _ = Nothing

fromPgcontacts :: [[SqlValue]] -> [Contact]
fromPgcontacts = catMaybes . (fmap fromPgcontact)

pgcontacts :: IO [Contact]
pgcontacts = do
  conn    <- connectPostgreSQL "dbname=birthdays"
  result  <- quickQuery conn "SELECT * FROM contacts" []
  --TODO: disconnect and keep result anywhere else
  --disc    <- disconnect conn
  return (fromPgcontacts result)


main :: IO ()
main = pgcontacts >>= (contactsThatHaveBirthdayInNDays 8) >>= print
