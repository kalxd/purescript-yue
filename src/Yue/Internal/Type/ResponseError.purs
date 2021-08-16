module Yue.Internal.Type.ResponseError where

import Data.Argonaut.Core (Json, fromString)

class IsResponseError e where
  errorStatus :: e -> Int
  errorContent :: e -> Json

status500 :: Int
status500 = 500

instance IsResponseError String where
  errorStatus _ = status500
  errorContent = fromString
