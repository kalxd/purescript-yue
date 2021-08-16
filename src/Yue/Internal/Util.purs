-- | 内部函数。
module Yue.Internal.Util where

import Prelude

import Data.Argonaut.Core (stringify) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Array (mapMaybe)
import Data.String (Pattern(..), split, trim)
import Data.String.NonEmpty (NonEmptyString, fromString)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Response, responseAsStream, setHeader)
import Node.Stream (end, writeString)

-- | 把一条String，根据"/"分割成多条非空String，自动过滤掉空组。
-- |
-- | ```
-- | splitString "a/c" = ["a", "c"]
-- | splitString "/a//c" = ["a", "c"]
-- | ```
splitString :: String -> Array NonEmptyString
splitString = mapMaybe fromString <<< split (Pattern "/") <<< trim

-- | 设置响应文本。
setResponseText :: Response -> String -> Effect Unit
setResponseText res s = do
  let r = responseAsStream res
  void $ writeString r UTF8 s $ pure unit
  end r $ pure unit

-- | 以JSON格式设置响应文本。
setResponseJson :: forall a. JSON.EncodeJson a => Response -> a -> Effect Unit
setResponseJson res = setResponseText res <<< JSON.stringify <<< JSON.encodeJson

-- | 设置默认的响应头，默认为application/json。
setResponseDefHeader :: Response -> Effect Unit
setResponseDefHeader res = do
  setHeader res "Content-Type" "appliction/json"
