module Birthdays.Wai(
  parsePath,
  queryParam,
  responseWithString,
  responseOK,
  responseNotFound,
  responseNotAllowed
) where

import Network.Wai
import Network.HTTP.Types

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

import Data.ByteString.Char8 (pack, split, unpack)

import Data.List (find)

parsePath :: Request -> [String]
parsePath req = filter (/= "") $ fmap unpack $ (split '/') . rawPathInfo $ req

queryParam :: String -> Request -> Maybe String
queryParam name req = fmap unpack $ queryItem >>= snd
  where
    itemHasGivenName (key, value) = key == (pack name)
    queryItem                     = find itemHasGivenName $ queryString req

responseWithString :: Status -> String -> Response
responseWithString status body = responseBuilder status [] (fromString body)

responseOK :: String -> Response
responseOK = responseWithString status200

responseNotFound :: String -> Response
responseNotFound = responseWithString status404

responseNotAllowed :: String -> Response
responseNotAllowed = responseWithString status405

