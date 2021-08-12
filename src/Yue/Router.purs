module Yue.Router ( RouterMap
                  , getRouter
                  ) where

import Prelude

import Data.Array (findMap)
import Data.Maybe (Maybe(..))
import Yue.Router.Path (RouterPath, stripPathPrefix)

type RouterKey = { path :: RouterPath
                 , method :: String
                 }

-- | 整个路由布局定义。
data RouterMap a = RouterNode RouterPath (Array (RouterMap a))
                 | RouterEnd RouterKey a

-- | 获取对应的路由Handler。
getRouter :: forall a. RouterKey -> RouterMap a -> Maybe a
getRouter key (RouterNode path xs) = do
  path' <- stripPathPrefix path key.path
  let key' = key { path = path' }
  findMap (getRouter key') xs
getRouter key (RouterEnd k a) | key == k = Just a
                              | otherwise = Nothing
