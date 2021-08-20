-- | 内部函数。
module Yue.Internal.Util where

import Prelude

import Data.Argonaut.Core (stringify) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.String (Pattern(..), split, trim)
import Data.String.NonEmpty (NonEmptyString, fromString)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestAsStream, responseAsStream, setHeader)
import Node.Stream as S

-- | 把一条String，根据"/"分割成多条非空String，自动过滤掉空组。
-- |
-- | ```
-- | splitString "a/c" = ["a", "c"]
-- | splitString "/a//c" = ["a", "c"]
-- | ```
splitString :: String -> Array NonEmptyString
splitString = mapMaybe fromString <<< split (Pattern "/") <<< trim

getRequestBodyText :: Request -> Aff String
getRequestBodyText req = makeAff f
  where r = requestAsStream req
        f k = do
          buffer <- Ref.new ""
          let ok s = Ref.modify_ (_ <> s) buffer
              no = k <<< Left
              finish = Ref.read buffer >>= k <<< Right
          S.onDataString r UTF8 ok
          S.onEnd r finish
          S.onError r no
          pure nonCanceler

-- | 设置响应文本。
setResponseText :: Response -> String -> Effect Unit
setResponseText res s = do
  let r = responseAsStream res
  void $ S.writeString r UTF8 s $ pure unit
  S.end r $ pure unit

-- | 以JSON格式设置响应文本。
setResponseJson :: forall a. JSON.EncodeJson a => Response -> a -> Effect Unit
setResponseJson res = setResponseText res <<< JSON.stringify <<< JSON.encodeJson

-- | 设置默认的响应头，默认为application/json。
setResponseDefHeader :: Response -> Effect Unit
setResponseDefHeader res = do
  setHeader res "Content-Type" "appliction/json"
