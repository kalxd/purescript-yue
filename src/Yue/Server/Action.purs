module Yue.Server.Action ( getURL
                         , getURLString
                         , setText
                         , setJson
                         , module E
                         ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Effect.Class (class MonadEffect, liftEffect)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Util (setResponseDefHeader, setResponseJson, setResponseText)

import Yue.Server.Header (tryHeader, header, setHeader) as E
import Yue.Server.Param (tryParam, param) as E
import Yue.Server.Query (tryQuery, query) as E
import Yue.Server.Control (finish, throw, throwE) as E

askRequest :: forall e m. Monad m => ActionT e m Request
askRequest = asks _.req

askResponse :: forall e m. Monad m => ActionT e m Response
askResponse = asks _.res

-- | 获取当前访问地址对象。
getURL :: forall e m. Monad m => ActionT e m URL
getURL = asks _.url

-- | 获取当前访问原始地址。
getURLString :: forall e m. Monad m => ActionT e m String
getURLString = requestURL <$> askRequest

-- | 设置响应体。
setText :: forall e m. MonadEffect m => String -> ActionT e m Unit
setText text = do
  res <- askResponse
  liftEffect $ setResponseText res text
  E.finish

-- | 设置响应体。
setJson :: forall e m a. EncodeJson a => MonadEffect m => a -> ActionT e m Unit
setJson a = do
  res <- askResponse
  liftEffect do
    setResponseDefHeader res
    setResponseJson res a
  E.finish
