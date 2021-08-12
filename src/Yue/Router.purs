module Yue.Router ( RouterMap
                  , getRouter
                  , get
                  , post
                  , put
                  , delete
                  ) where

import Prelude

import Data.Array (findMap)
import Data.Maybe (Maybe(..))
import Yue.Router.Action (ActionT)
import Yue.Router.Path (RouterPath, mapToRouterPath, stripPathPrefix)

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

route' :: forall s m a. String -> String -> ActionT s m a -> RouterMap (ActionT s m a)
route' method path = RouterEnd key
  where path' = mapToRouterPath path
        key = { path: path', method }

type RouterApp = forall s m a. String -> ActionT s m a -> RouterMap (ActionT s m a)

get :: RouterApp
get = route' "GET"

post :: RouterApp
post = route' "POST"

put :: RouterApp
put = route' "PUT"

delete :: RouterApp
delete = route' "DELETE"
