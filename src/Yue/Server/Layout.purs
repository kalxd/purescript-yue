module Yue.Server.Layout ( Layout
                         , getRouter
                         , get
                         , post
                         , put
                         , delete
                         ) where

import Prelude

import Data.Array (findMap)
import Data.Maybe (Maybe(..))
import Yue.Action (ActionT)
import Yue.Internal.Router.Path (RouterPath, mapToRouterPath, stripPathPrefix)

type LayoutKey = { path :: RouterPath
                 , method :: String
                 }

-- | 整个路由布局定义。
data Layout a = RouterNode RouterPath (Array (Layout a))
              | RouterEnd LayoutKey a

-- | 获取对应的路由Handler。
getRouter :: forall a. LayoutKey -> Layout a -> Maybe a
getRouter key (RouterNode path xs) = do
  path' <- stripPathPrefix path key.path
  let key' = key { path = path' }
  findMap (getRouter key') xs
getRouter key (RouterEnd k a) | key == k = Just a
                              | otherwise = Nothing

route' :: forall s m a. String -> String -> ActionT s m a -> Layout (ActionT s m a)
route' method path = RouterEnd key
  where path' = mapToRouterPath path
        key = { path: path', method }

type RouterApp = forall s m a. String -> ActionT s m a -> Layout (ActionT s m a)

get :: RouterApp
get = route' "GET"

post :: RouterApp
post = route' "POST"

put :: RouterApp
put = route' "PUT"

delete :: RouterApp
delete = route' "DELETE"
