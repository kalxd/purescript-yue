module Yue.Internal.Type.Path where

import Prelude

import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString, stripPrefix)
import Data.String.Pattern (Pattern(..))
import Yue.Internal.Util (splitString)

-- | 请求的路径，自动转化成分片形式。
newtype RequestPath = RequestPath (List NonEmptyString)

mkRequestPath :: String -> RequestPath
mkRequestPath = RequestPath <<< fromFoldable <<< splitString

-- | 路由的定义，定义中允许动态变量存在。
data RouterSegment = RouterLit NonEmptyString
                   | RouterParam NonEmptyString

type RouterPath = List RouterSegment

toRouterSegment :: NonEmptyString -> RouterSegment
toRouterSegment s = case stripPrefix (Pattern ":") s of
  Just s' -> RouterParam s'
  Nothing -> RouterLit s

toRouterPath :: String -> RouterPath
toRouterPath = map toRouterSegment <<< fromFoldable <<< splitString
