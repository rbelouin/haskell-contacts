module Birthdays.Contact.PostgreSQL (
  contactFromSQL,
  contactsFromSQL,
  contactsPostgreSQL
) where

import Data.ByteString.Char8
import Data.Maybe

import Birthdays.Contact
import Birthdays.Time

import Database.HDBC
import Database.HDBC.PostgreSQL

contactFromSQL :: [SqlValue] -> Maybe Contact
contactFromSQL [SqlByteString id, SqlByteString lastName, SqlByteString firstName, SqlLocalDate birthDate] = Just $ Contact (unpack id) (unpack lastName) (unpack firstName) birthDate
contactFromSQL _ = Nothing

contactsFromSQL :: [[SqlValue]] -> [Contact]
contactsFromSQL = catMaybes . (fmap contactFromSQL)

contactsPostgreSQL :: IO [Contact]
contactsPostgreSQL = do
  conn    <- connectPostgreSQL "dbname=birthdays"
  result  <- quickQuery conn "SELECT id, lastname, firstname, birthdate FROM contacts" []
  --TODO: disconnect and keep result anywhere else
  --disc    <- disconnect conn
  return (contactsFromSQL result)
