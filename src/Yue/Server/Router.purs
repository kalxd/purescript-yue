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
import Data.List (uncons)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Node.HTTP (requestMethod)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Error (packErrorResponse)
import Yue.Internal.Type.MatchState (MatchState(..), insertParamMap, unconsPath)
import Yue.Internal.Type.Method (Method(..), fromString) as I
import Yue.Internal.Type.Path (RequestPath(..), RouterPath, RouterSegment(..), toRouterPath)
import Yue.Server.Body (setJson)
import Yue.Server.Header (setCode)

-- 准确匹配。
matchExactPath :: MatchState -> RouterPath -> Maybe (MatchState)
matchExactPath state@(MatchState { path: RequestPath Nil }) Nil = Just state
matchExactPath state xs = do
  ms <- unconsPath state
  x <- uncons xs
  case x.head of
    RouterParam key -> let s' = insertParamMap key ms.head ms.rest
                        in matchExactPath s' x.tail
    RouterLit name | name == ms.head  -> matchExactPath ms.rest x.tail
                   | otherwise -> Nothing

-- 将请求地址与路由地址进行匹配。
matchPrefixPath :: MatchState -> RouterPath -> Maybe (MatchState)
matchPrefixPath rs Nil = Just rs
matchPrefixPath state xs = do
  ms <- unconsPath state
  x <- uncons xs
  case x.head of
    RouterParam key -> let s' = insertParamMap key ms.head ms.rest
                        in matchPrefixPath s' x.tail
    RouterLit name | name == ms.head  -> matchPrefixPath ms.rest x.tail
                   | otherwise -> Nothing

-- | 匹配满足访问地址的路由，不对method作区别。
-- | 前面满足即可。
route :: forall e m a. Monad m => String -> ActionT e m a -> ActionT e m Unit
route path action = do
  s <- TR.get
  let routerPath = toRouterPath path
  case matchPrefixPath s routerPath of
    Just s' -> do
      TR.put s'
      void $ action
    Nothing -> pure unit

matchMethod :: forall e m a. Monad m => I.Method -> String -> ActionT e m a -> ActionT e m Unit
matchMethod method path action = do
  req <- TR.asks _.req
  s <- TR.get
  let routerPath = toRouterPath path
  when (Just method == (I.fromString $ requestMethod req)) do
    case matchExactPath s routerPath of
      Just s' -> do
        TR.put s'
        void $ action
      Nothing -> pure unit

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
  setCode 404
  setJson $ packErrorResponse "api不存在"
