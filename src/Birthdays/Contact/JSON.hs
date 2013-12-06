module Birthdays.Contact.JSON(
  contactFromJSValue,
  contactToJSValue
) where

import Prelude hiding (id)

import Birthdays.Contact
import Birthdays.JSON
import Birthdays.Time

instance JSON Contact where
  readJSON = contactFromJSValue
  showJSON = contactToJSValue

contactFromJSValue :: JSValue -> Result Contact
contactFromJSValue jsvalue = case jsvalue of
  JSObject o  -> contactFromJSObject o
  _           -> Error "jvalue is not an object"
  where 
    contactFromJSObject o = maybe (Error "Your JSON data is invalid") Ok $ (do
      id        <- get_string "id"        o
      lastName  <- get_string "lastName"  o
      firstName <- get_string "firstName" o
      birthDate <- get_day    "birthDate" o
      return $ Contact id lastName firstName birthDate)

contactToJSValue :: Contact -> JSValue
contactToJSValue contact = JSObject $ toJSObject $  [("id",       (JSString . toJSString . id) contact)
                                                    ,("lastName", (JSString . toJSString . lastName) contact)
                                                    ,("firstName",(JSString . toJSString . firstName) contact)
                                                    ,("birthDate",(JSString . toJSString . showGregorian . birthDate) contact)]
