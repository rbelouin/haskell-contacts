import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (Status, status200, status404)

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Data.ByteString.Char8 (split, unpack)

import Birthdays.Contact
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


parsePath :: Request -> [String]
parsePath req = filter (/= "") $ fmap unpack $ (split '/') . rawPathInfo $ req


-- ROUTING
router :: Request -> IO Response
router req = case parsePath req of
  ["contacts"]  -> fmap (sendOK . show) contacts
  _             -> return $ sendNotFound "Not found."


main :: IO ()
main = do
  putStrLn $ "Listening on port " ++ (show port)
  run port router
