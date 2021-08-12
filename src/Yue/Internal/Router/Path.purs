module Yue.Router.Internal.Path ( RouterSegment(..)
                                , RouterPath
                                , mapToRouterPath
                                , stripPathPrefix
                                ) where

import Prelude

import Data.Array (mapMaybe, uncons)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString

-- | 路径描述。
data RouterSegment = RouterLit NonEmptyString -- | 纯文本路径，不需要解析。
                   | RouterParam NonEmptyString -- | 需要解析的文本。

derive instance Eq RouterSegment

instance Show RouterSegment where
  show (RouterLit s) = ":" <> NonEmptyString.toString s
  show (RouterParam s) = NonEmptyString.toString s

type RouterPath = Array RouterSegment

-- 把一条String，根据"/"分割成多条非空String，自动过滤掉空组。
--
-- splitString "a/c" = ["a", "c"]
-- splitString "/a//c" = ["a", "c"]
splitString :: String -> Array NonEmptyString
splitString = mapMaybe NonEmptyString.fromString <<< split (Pattern "/") <<< trim

-- 将路径小一块转化成`RouterPath`。
toSegment :: NonEmptyString.NonEmptyString -> RouterSegment
toSegment s = case NonEmptyString.stripPrefix (Pattern ":") s of
  Just a -> RouterParam a
  Nothing -> RouterLit s

-- | 将外部的`String`转化成`RouterPath`。
mapToRouterPath :: String -> RouterPath
mapToRouterPath = map toSegment <<< splitString

-- | 过滤前半相同部分，留下剩余未匹配部分。
stripPathPrefix :: RouterPath -> RouterPath -> Maybe RouterPath
stripPathPrefix [] [] = Just []
stripPathPrefix [] xs = Just xs
stripPathPrefix xs ys = do
  x <- uncons xs
  y <- uncons ys
  if x.head == y.head
    then stripPathPrefix x.tail y.tail
    else Nothing
