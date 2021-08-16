-- | 请求上下文定义。
module Yue.Internal.Type.Action where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL, parse)
import Yue.Internal.Type.Error (ActionError)
import Yue.Internal.Type.MatchState (MatchState)
import Yue.Internal.Type.Query (Query, mkQuery)

-- | 一条请求因何而中断。
data ActionST e = ActionFinish -- ^ 中断信号。
                | ActionInnerError ActionError -- ^ 内部错误。
                | ActionError e -- ^ 用户自定义错误。

-- | 请求的全部原始上下文，其中也包含预处理过的信息，如query、path。
type ActionEnv = { req :: Request
                 , res :: Response
                 , url :: URL
                 , query :: Maybe (Query String)
                 }

mkActionEnv :: Request -> Response -> ActionEnv
mkActionEnv req res = { req, res, url, query }
  where url = parse $ requestURL req
        query = mkQuery <$> toMaybe url.query

-- | 最核心的类型，这是一个路由的定义。
type ActionT e m = ExceptT (ActionST e) (ReaderT ActionEnv (StateT MatchState m))
