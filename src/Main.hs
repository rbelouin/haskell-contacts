import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Data.ByteString.Char8 (ByteString, pack, split, unpack)

import Data.List
import Data.Maybe

import Birthdays.JSON

import Birthdays.Contact
import Birthdays.Contact.JSON

import Birthdays.Contact.PostgreSQL (contactsPostgreSQL)
import Birthdays.Contact.Static     (contactsStatic)

-- STORAGE BACKEND
contacts :: IO [Contact]
--contacts = contactsPostgreSQL
contacts = return contactsStatic


-- WEB SERVER PORT
port :: Int
port = 8080


send :: Status -> String -> Response
send status body = responseBuilder status [] (fromString body)

sendOK :: String -> Response
sendOK = send status200

sendNotFound :: String -> Response
sendNotFound = send status404

sendNotAllowed :: String -> Response
sendNotAllowed = send status405


parsePath :: Request -> [String]
parsePath req = filter (/= "") $ fmap unpack $ (split '/') . rawPathInfo $ req


parseInteger :: String -> Maybe Integer
parseInteger string = case (reads string) of
  [(n, "")] -> Just n
  _         -> Nothing


getParameter :: String -> Request -> Maybe String
getParameter name req = fmap unpack $ queryItem >>= snd
  where
    itemHasGivenName (key, value) = key == (pack name)
    queryItem                     = find itemHasGivenName $ queryString req

getParameterOrElse :: String -> String -> Request -> String
getParameterOrElse defaultValue name req = fromMaybe defaultValue $ getParameter name req


getContacts :: Application
getContacts req = fmap (sendOK . encode) fContacts
  where
    fContacts                 = maybe contacts (thatHaveBD contacts) days
    thatHaveBD contacts days  = contacts >>= (contactsThatHaveBirthdayInNDays days)
    days                      = (getParameter "days" req) >>= parseInteger

-- ROUTING
router :: Request -> IO Response
router req = case parsePath req of
  ["contacts"]  -> contactsRouter req
  _             -> return $ sendNotFound "Not found."
  where
    contactsRouter req = case (unpack $ requestMethod req) of
      "GET" -> getContacts req
      _     -> return $ sendNotAllowed "Not Allowed."

main :: IO ()
main = do
  putStrLn $ "Listening on port " ++ (show port)
  run port router
