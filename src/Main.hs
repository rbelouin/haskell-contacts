import Network.Wai
import Network.Wai.Handler.Warp

import Data.ByteString.Char8 (unpack)

import Birthdays.JSON

import Birthdays.Contact
import Birthdays.Contact.JSON

import Birthdays.Contact.PostgreSQL (contactsPostgreSQL)
import Birthdays.Contact.Static     (contactsStatic)

import Birthdays.Wai

-- STORAGE BACKEND
contacts :: IO [Contact]
--contacts = contactsPostgreSQL
contacts = return contactsStatic


-- WEB SERVER PORT
port :: Int
port = 8080


parseInteger :: String -> Maybe Integer
parseInteger string = case (reads string) of
  [(n, "")] -> Just n
  _         -> Nothing


getContacts :: Application
getContacts req = fmap (responseOK . encode) fContacts
  where
    fContacts                 = maybe contacts (thatHaveBD contacts) days
    thatHaveBD contacts days  = contacts >>= (contactsThatHaveBirthdayInNDays days)
    days                      = (queryParam "days" req) >>= parseInteger

-- ROUTING
router :: Request -> IO Response
router req = case parsePath req of
  ["contacts"]  -> contactsRouter req
  _             -> return $ responseNotFound "Not found."
  where
    contactsRouter req = case (unpack $ requestMethod req) of
      "GET" -> getContacts req
      _     -> return $ responseNotAllowed "Not Allowed."

main :: IO ()
main = do
  putStrLn $ "Listening on port " ++ (show port)
  run port router
