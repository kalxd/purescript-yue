module Yue.Internal.Type.Parsable ( class Parsable
                                  , parseParam
                                  ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Int as IInt
import Data.Maybe (Maybe(..))
import Data.Number as INumber

-- | 将String转化成其它格式。
class Parsable a where
  parseParam :: String -> Either String a

instance Parsable String where
  parseParam = Right

instance Parsable Number where
  parseParam s = note msg $ INumber.fromString s
    where msg = s <> "无法转化为Number"

instance Parsable Int where
  parseParam s = note msg $ IInt.fromString s
    where msg = s <> "无法转化为Int"

instance Parsable a => Parsable (Maybe a) where
  parseParam s = case parseParam s of
    Right a -> Right a
    Left _ -> Right Nothing
