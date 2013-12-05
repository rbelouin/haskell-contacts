import Birthdays.Contact
import Birthdays.Contact.PostgreSQL (contactsPostgreSQL)
import Birthdays.Contact.Static     (contactsStatic)
import Birthdays.Time

contacts :: IO [Contact]
--contacts = contactsPostgreSQL
contacts = return contactsStatic

main :: IO ()
main = contacts >>= (contactsThatHaveBirthdayInNDays 8) >>= print
