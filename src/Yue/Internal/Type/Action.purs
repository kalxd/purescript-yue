-- | 请求上下文定义。
module Yue.Internal.Type.Action where

import Prelude

import Control.Monad.Except.Trans (ExceptT, catchError, throwError)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL, parse)
import Yue.Internal.Type.Error (YueError(..))
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

throwChecked :: forall e m a. Monad m => String -> ActionT e m a
throwChecked = throwError <<< ActionChecked <<< YueError

throwCheckedMaybe :: forall e m a. Monad m => String -> Maybe a -> ActionT e m a
throwCheckedMaybe e Nothing = throwChecked e
throwCheckedMaybe _ (Just a) = pure a

-- 作用同`except`一致，它只是包装了一层而已。
exceptEither :: forall e m a b. Monad m => Show b => Either b a -> ActionT e m a
exceptEither (Left e) = throwChecked $ show e
exceptEither (Right a) = pure a

catchAction :: forall e m a. Monad m => ActionT e m a -> ActionT e m (Maybe a)
catchAction action = (Just <$> action) `catchError` f
  where f _ = pure Nothing

fromMaybeAction :: forall e m a. Monad m => ActionT e m (Maybe a) -> a -> ActionT e m a
fromMaybeAction action a = fromMaybe a <$> action
