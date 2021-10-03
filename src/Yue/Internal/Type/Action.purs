-- | 请求上下文定义。
module Yue.Internal.Type.Action where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Data.Maybe (Maybe, fromMaybe)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL, parse)
import Yue.Internal.Type.Error (YueError)
import Yue.Internal.Type.MatchState (MatchState)

-- | 一条请求因何而中断。
data ActionST e = ActionFinish -- ^ 中断信号。
                | ActionChecked YueError -- ^ 已知的内部错误。
                | ActionError e -- ^ 用户自定义错误。

-- | 请求的全部原始上下文，其中也包含预处理过的信息，如query、path。
type ActionEnv = { req :: Request
                 , res :: Response
                 , url :: URL
                 }

mkActionEnv :: Request -> Response -> ActionEnv
mkActionEnv req res = { req, res, url }
  where url = parse $ requestURL req

-- | 最核心的类型，这是一个路由的定义。
type ActionT e m = ExceptT (ActionST e) (ReaderT ActionEnv (StateT MatchState m))

fromMaybeAction :: forall e m a. Monad m => ActionT e m (Maybe a) -> a -> ActionT e m a
fromMaybeAction action a = fromMaybe a <$> action
