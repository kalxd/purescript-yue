module Yue.Parsable ( tryParseParam
                    , parseParam
                    , class IsParamParsable) where

import Prelude

import Data.Either (Either(..), hush, note)
import Data.Int as IInt
import Data.Maybe (Maybe)
import Data.Number as INumber

-- | 将一小截param参数，转化成必要的类型。
class IsParamParsable a where
  parseParam :: String -> Either String a

instance IsParamParsable String where
  parseParam = Right

instance IsParamParsable Number where
  parseParam s = note msg $ INumber.fromString s
    where msg = s <> "无法转化为Number"

instance IsParamParsable Int where
  parseParam s = note msg $ IInt.fromString s
    where msg = s <> "无法转化为Int"

tryParseParam :: forall a. IsParamParsable a => String -> Maybe a
tryParseParam = hush <<< parseParam
