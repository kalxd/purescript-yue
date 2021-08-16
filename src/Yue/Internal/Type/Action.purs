-- | 请求上下文定义。
module Yue.Internal.Type.Action where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Data.HashMap (HashMap, empty)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL, parse)
import Yue.Internal.Type.Path (RequestPath, mkRequestPath)
import Yue.Internal.Type.Query (Query, mkQuery)

-- | 一条请求因何而中断。
data ActionST = ActionFinish
              | ActionError

-- | 请求的全部原始上下文，其中也包含预处理过的信息，如query、path。
type ActionEnv = { req :: Request
                 , res :: Response
                 , url :: URL
                 , query :: Maybe (Query String)
                 }

-- | 每个请求内部的状态。
type ActionState = { path :: RequestPath
                   , paramHash :: HashMap String String
                   }

mkActionEnv :: Request -> Response -> ActionEnv
mkActionEnv req res = { req, res, url, query }
  where url = parse $ requestURL req
        query = mkQuery <$> toMaybe url.query

initActionState :: String -> ActionState
initActionState p = { path, paramHash }
  where path = mkRequestPath p
        paramHash = empty

-- | 最核心的类型，这是一个路由的定义。
type ActionT m = ExceptT ActionST (ReaderT ActionEnv (StateT ActionState m))
