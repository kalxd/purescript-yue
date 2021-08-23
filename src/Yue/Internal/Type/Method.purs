module Yue.Internal.Type.Method where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toUpper)

data Method = GET
            | POST
            | PUT
            | PATCH
            | DELETE

instance Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show PATCH = "PATCH"
  show DELETE = "DELETE"

derive instance Eq Method

fromString :: String -> Maybe Method
fromString s = case toUpper s of
  "GET" -> Just GET
  "POST" -> Just POST
  "PUT" -> Just PUT
  "PATCH" -> Just PATCH
  "DELETE" -> Just DELETE
  _ -> Nothing
