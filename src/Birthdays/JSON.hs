module Birthdays.JSON(
  get_string,
  get_day,
  JSON(..),
  JSValue(..),
  JSObject(..),
  toJSObject,
  JSString(..),
  toJSString,
  Result(..),
  encode
) where 
  
import Text.JSON
import Text.JSON.Types

import Data.Time.Format
import System.Locale

import Birthdays.Time

get_string :: String -> JSObject JSValue -> Maybe String
get_string key object = (get_field object key) >>= (\f -> case f of
  JSString s  -> Just $ fromJSString s
  _           -> Nothing)

get_day :: String -> JSObject JSValue -> Maybe Day
get_day key object = get_string key object >>= parseTime defaultTimeLocale "%F"
