-- | 内部函数。
module Yue.Internal.Util where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (mapMaybe)
import Data.String (Pattern(..), split, trim)
import Data.String.NonEmpty (NonEmptyString, fromString)

-- | 把一条String，根据"/"分割成多条非空String，自动过滤掉空组。
-- |
-- | ```
-- | splitString "a/c" = ["a", "c"]
-- | splitString "/a//c" = ["a", "c"]
-- | ```
splitString :: String -> Array NonEmptyString
splitString = mapMaybe fromString <<< split (Pattern "/") <<< trim

parseJSON :: forall a. EncodeJson a => a -> String
parseJSON = stringify <<< encodeJson
