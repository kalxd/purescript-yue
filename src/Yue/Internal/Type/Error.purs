-- | 私货特别严重的错误处理。
module Yue.Internal.Type.Error where

import Control.Category ((<<<))
import Data.Argonaut.Core (Json, jsonSingletonObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Yue.Internal.Type.ResponseError (class IsResponseError, errorStatus)

data AppError e = AppUnexcept String -- ^ 未知错误，不想处理的错误都可以扔到这里。
                | AppOther e

packErrorResponse :: forall a. EncodeJson a => a -> Json
packErrorResponse = jsonSingletonObject "err" <<< encodeJson

instance EncodeJson e => EncodeJson (AppError e) where
  encodeJson (AppUnexcept s) = packErrorResponse s
  encodeJson (AppOther e) = packErrorResponse e

instance (IsResponseError e, EncodeJson e) => IsResponseError (AppError e) where
  errorStatus (AppUnexcept _) = 500
  errorStatus (AppOther e) = errorStatus e

  errorContent = encodeJson
