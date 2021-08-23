module Yue.Server.Router ( route
                         , get
                         , post
                         , put
                         , patch
                         , delete
                         , notFound
                         ) where

import Prelude

import Control.Monad.Reader.Trans (asks) as TR
import Control.Monad.State.Trans (get, put) as TR
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Node.HTTP (requestMethod)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.MatchState (MatchState(..), insertParamMap, setMatchPath)
import Yue.Internal.Type.Method (Method(..), fromString) as I
import Yue.Internal.Type.Path (RequestPath(..), RouterPath, RouterSegment(..), toRouterPath)
import Yue.Server.Body (setJson)
import Yue.Server.Control (finish)

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
  s <- TR.get
  let routerPath = toRouterPath path
  case matchRouterPath s routerPath of
    Just s' -> do
      TR.put s'
      void $ action
      finish
    Nothing -> pure unit

matchMethod :: forall e m a. Monad m => I.Method -> String -> ActionT e m a -> ActionT e m Unit
matchMethod method path action = do
  req <- TR.asks _.req
  when (Just method == (I.fromString $ requestMethod req)) do
    route path action

get :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
get = matchMethod I.GET

post :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
post = matchMethod I.POST

put :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
put = matchMethod I.PUT

patch :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
patch = matchMethod I.PATCH

delete :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
delete = matchMethod I.DELETE

notFound :: forall e m. MonadEffect m => ActionT e m Unit
notFound = do
  setJson $ "api不存在"
