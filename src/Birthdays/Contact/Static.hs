module Birthdays.Contact.Static(
  contactsStatic
) where

import Birthdays.Contact
import Birthdays.Time

contactsStatic :: [Contact]
contactsStatic = [Contact "1" "JAMAL" "Ahmad" (fromGregorian 1930 7 2), Contact "2" "ROMANO" "Aldo" (fromGregorian 1941 1 16), Contact "3" "BRUBECK" "Dave" (fromGregorian 1920 12 6)]
