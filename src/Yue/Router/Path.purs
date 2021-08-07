module Yue.Router.Path ( RouterSegment(..)
                       , RouterPath
                       , mapToRouterPath) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Yue.Internal.Util (splitString)

-- | 路径描述。
data RouterSegment = RouterLit NonEmptyString -- | 纯文本路径，不需要解析。
                   | RouterParam NonEmptyString -- | 需要解析的文本。

type RouterPath = Array RouterSegment

-- 将路径小一块转化成`RouterPath`。
mapRouterSegment :: NonEmptyString.NonEmptyString -> RouterSegment
mapRouterSegment s = case NonEmptyString.stripPrefix (NonEmptyString.Pattern ":") s of
  Just a -> RouterParam a
  Nothing -> RouterLit s

-- | 将外部的`String`转化成`RouterPath`。
mapToRouterPath :: String -> RouterPath
mapToRouterPath = map mapRouterSegment <<< splitString
