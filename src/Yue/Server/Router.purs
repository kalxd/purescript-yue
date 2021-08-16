module Yue.Server.Router (route) where

import Prelude

import Control.Monad.State.Trans (get, put)
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.MatchState (MatchState(..), initMatchState, insertParamMap)
import Yue.Internal.Type.Path (RequestPath(..), RouterPath, RouterSegment(..), toRouterPath)
import Yue.Server.Action (finish)

-- 将请求地址与路由地址进行匹配。
-- 如果前半部分相同，那么得到(Just 剩余地址)，反之得到`Nothing`。
matchRouterPath :: RequestPath -> RouterPath -> Maybe (MatchState)
matchRouterPath rs [] = Just $ initMatchState rs
matchRouterPath (RequestPath []) _ = Nothing
matchRouterPath (RequestPath rs) xs = do
  r <- uncons rs
  x <- uncons xs
  case x.head of
    RouterParam key -> let f = insertParamMap key r.head
                        in f <$> matchRouterPath (RequestPath r.tail) x.tail
    RouterLit name -> if r.head == name
                      then matchRouterPath (RequestPath r.tail) x.tail
                      else Nothing


-- | 匹配满足访问地址的路由，不对method作区别。
route :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
route path action = do
  (MatchState s) <- get
  let routerPath = toRouterPath path
  case matchRouterPath s.path routerPath of
    Just s' -> do
      put s'
      void $ action
      finish
    Nothing -> pure unit
