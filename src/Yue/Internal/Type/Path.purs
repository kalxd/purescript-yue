module Yue.Internal.Type.Path where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString, stripPrefix)
import Data.String.Pattern (Pattern(..))
import Yue.Internal.Util (splitString)

-- | 请求的路径，自动转化成分片形式。
newtype RequestPath = RequestPath (Array NonEmptyString)

mkRequestPath :: String -> RequestPath
mkRequestPath = RequestPath <<< splitString

-- | 路由的定义，定义中允许动态变量存在。
data RouterSegment = RouterLit NonEmptyString
                   | RouterParam NonEmptyString

type RouterPath = Array RouterSegment

toRouterSegment :: NonEmptyString -> RouterSegment
toRouterSegment s = case stripPrefix (Pattern ":") s of
  Just s' -> RouterParam s'
  Nothing -> RouterLit s

toRouterPath :: String -> RouterPath
toRouterPath = map toRouterSegment <<< splitString

-- | 将请求地址与路由地址进行匹配。
-- | 如果前半部分相同，那么得到(Just 剩余地址)，反之得到`Nothing`。
matchRouterPath :: RequestPath -> RouterPath -> Maybe (RequestPath)
matchRouterPath rs [] = Just rs
matchRouterPath (RequestPath []) _ = Nothing
matchRouterPath (RequestPath rs) xs = do
  r <- uncons rs
  x <- uncons xs
  case x.head of
    RouterParam _ -> matchRouterPath (RequestPath r.tail) x.tail
    RouterLit name -> if r.head == name
                      then matchRouterPath (RequestPath r.tail) x.tail
                      else Nothing
