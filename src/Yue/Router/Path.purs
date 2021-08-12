module Yue.Router.Path ( RouterSegment(..)
                       , RouterPath
                       , mapToRouterPath
                       , stripPathPrefix
                       ) where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Yue.Internal.Util (splitString)

-- | 路径描述。
data RouterSegment = RouterLit NonEmptyString -- | 纯文本路径，不需要解析。
                   | RouterParam NonEmptyString -- | 需要解析的文本。

derive instance Eq RouterSegment

instance Show RouterSegment where
  show (RouterLit s) = ":" <> NonEmptyString.toString s
  show (RouterParam s) = NonEmptyString.toString s

type RouterPath = Array RouterSegment

-- 将路径小一块转化成`RouterPath`。
mapRouterSegment :: NonEmptyString.NonEmptyString -> RouterSegment
mapRouterSegment s = case NonEmptyString.stripPrefix (NonEmptyString.Pattern ":") s of
  Just a -> RouterParam a
  Nothing -> RouterLit s

-- | 将外部的`String`转化成`RouterPath`。
mapToRouterPath :: String -> RouterPath
mapToRouterPath = map mapRouterSegment <<< splitString

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
