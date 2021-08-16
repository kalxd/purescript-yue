module Yue.Internal.Type.Parsable ( class IsParamParsable
                                  , parseParam
                                  ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Int as IInt
import Data.Maybe (Maybe(..))
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

instance IsParamParsable a => IsParamParsable (Maybe a) where
  parseParam s = case parseParam s of
    Right a -> Right a
    Left _ -> Right Nothing
