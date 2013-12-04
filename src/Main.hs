import Data.List
import Data.Time.Calendar

data Contact = Contact {
  id :: String,
  lastName :: String,
  firstName :: String,
  birthDate :: Day
} deriving Show

contacts :: [Contact]
contacts = [Contact "1" "JAMAL" "Ahmad" (fromGregorian 1930 7 2), Contact "2" "ROMANO" "Aldo" (fromGregorian 1941 1 16)]

main :: IO ()
main = print contacts
