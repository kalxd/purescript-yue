-- | 私货特别严重的错误处理。
module Yue.Internal.Type.Error where

import Prelude

import Data.Argonaut.Core (Json, fromString, jsonSingletonObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Yue.Internal.Type.ResponseError (class IsResponseError, errorStatus)

-- | 用户自定义错误。
newtype AppError e = AppError e

packErrorResponse :: forall a. EncodeJson a => a -> Json
packErrorResponse = jsonSingletonObject "err" <<< encodeJson

instance EncodeJson e => EncodeJson (AppError e) where
  encodeJson (AppError e) = packErrorResponse e

instance (IsResponseError e, EncodeJson e) => IsResponseError (AppError e) where
  errorStatus (AppError e) = errorStatus e

  errorContent = encodeJson

-- | 内部错误状态，用于处理内部错误，不受用户干预，也无须干预。
-- | 由于属于错误范畴，需要中断请求并返回结果。
newtype YueError = YueError String -- ^ 请求参数不正确。

instance Show YueError where
  show (YueError s) = s

instance IsResponseError YueError where
  errorStatus _ = 401
  errorContent = fromString <<< show
