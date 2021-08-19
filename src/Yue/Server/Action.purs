module Yue.Server.Action ( getURL
                         , getURLString
                         , tryQuery
                         , query
                         , setText
                         , setJson
                         , finish
                         , throw
                         , throwE
                         , module E
                         ) where

import Prelude

import Control.Monad.Except.Trans (except, throwError, withExceptT)
import Control.Monad.Reader.Trans (asks)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL)
import Yue.Internal.Type.Action (ActionST(..), ActionT, throwChecked)
import Yue.Internal.Type.Error (AppError(..), YueError(..))
import Yue.Internal.Type.Parsable (class Parsable, parseParam)
import Yue.Internal.Type.Query (lookupQuery, mkQuery)
import Yue.Internal.Util (setResponseDefHeader, setResponseJson, setResponseText)

import Yue.Server.Param (tryParam, param) as E
import Yue.Server.Header (tryHeader, header, setHeader) as E

askRequest :: forall e m. Monad m => ActionT e m Request
askRequest = asks _.req

askResponse :: forall e m. Monad m => ActionT e m Response
askResponse = asks _.res

-- | 获取当前访问地址对象。
getURL :: forall e m. Monad m => ActionT e m URL
getURL = asks _.url

tryQuery :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryQuery key = do
  url <- getURL
  case lookupQuery key =<< mkQuery <$> toMaybe url.query of
    Nothing -> pure Nothing
    Just v -> withExceptT (ActionChecked <<< YueError) $ except $ parseParam v

query :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
query key = do
  mv <- tryQuery key
  case mv of
    Just v -> pure v
    Nothing -> throwChecked $ "query参数" <> key <> "未提供。"

-- | 获取当前访问原始地址。
getURLString :: forall e m. Monad m => ActionT e m String
getURLString = requestURL <$> askRequest

-- | 设置响应体。
setText :: forall e m. MonadEffect m => String -> ActionT e m Unit
setText text = do
  res <- askResponse
  liftEffect $ setResponseText res text
  finish

-- | 设置响应体。
setJson :: forall e m a. EncodeJson a => MonadEffect m => a -> ActionT e m Unit
setJson a = do
  res <- askResponse
  liftEffect do
    setResponseDefHeader res
    setResponseJson res a
  finish

finish :: forall e m a. Monad m => ActionT e m a
finish = throwError ActionFinish

throw :: forall e m a. Monad m => e -> ActionT e m a
throw = throwError <<< ActionError

-- | 特供版，抛出的错误自动包装在`AppError`。
throwE :: forall e m a. Monad m => e -> ActionT (AppError e) m a
throwE = throw <<< AppError
