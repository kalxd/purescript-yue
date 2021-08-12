module Yue.Internal.Router.Path ( RouterSegment(..)
                                , RouterPath
                                , RequestPath
                                , mapToRouterPath
                                , stripPathPrefix
                                , urlToRequestPath
                                ) where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
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

-- | 一条真实的请求，分割成小块路径名。它被用于内部的路径比较。
-- | * 当一条进来时，首先从request url找出url path。
-- | * 将url path转化成小块列表的`RequestPath`。
-- | * 依次与已定义`RouterPath`对比。
-- |
-- | 与`RouterPath`不同的是，它不需要处理`:<name>`这样的格式。
-- |
-- | ```
-- | "/a/c" = ["a", "c"]
-- | "/a/:c" = ["a", ":c"]
-- | ```
newtype RequestPath = RequestPath (Array NonEmptyString)

urlToRequestPath :: String -> RequestPath
urlToRequestPath = RequestPath <<< splitString
