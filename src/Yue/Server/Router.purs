module Yue.Server.Router (route) where

import Prelude

import Control.Monad.State.Trans (get, put)
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.MatchState (MatchState(..), insertParamMap, setMatchPath)
import Yue.Internal.Type.Path (RequestPath(..), RouterPath, RouterSegment(..), toRouterPath)
import Yue.Server.Action (finish)

-- 将请求地址与路由地址进行匹配。
-- 实现有些乱。
matchRouterPath :: MatchState -> RouterPath -> Maybe (MatchState)
matchRouterPath rs [] = Just rs
matchRouterPath state@(MatchState s) xs = do
  let (RequestPath rs) = s.path
  r <- uncons rs
  x <- uncons xs
  case x.head of
    RouterParam key -> let s' = insertParamMap key r.head state
                           s'' = setMatchPath r.tail s'
                        in matchRouterPath s'' x.tail
    RouterLit name -> if r.head == name
                      then let s' = setMatchPath r.tail state
                           in matchRouterPath s' x.tail
                      else Nothing

-- | 匹配满足访问地址的路由，不对method作区别。
route :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
route path action = do
  s <- get
  let routerPath = toRouterPath path
  case matchRouterPath s routerPath of
    Just s' -> do
      put s'
      void $ action
      finish
    Nothing -> pure unit
