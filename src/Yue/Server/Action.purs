module Yue.Server.Action ( getURL
                         , getURLString
                         , tryQuery
                         , query
                         , tryParam
                         , param
                         , setText
                         , setJson
                         , finish
                         , throw
                         , throwE
                         ) where

import Prelude

import Control.Monad.Except.Trans (except, throwError, withExceptT)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.State.Trans (get, gets)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (hush)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Node.HTTP (Request, Response, requestURL)
import Node.URL (URL)
import Yue.Internal.Type.Action (ActionST(..), ActionT)
import Yue.Internal.Type.Error (ActionError(..), AppError(..))
import Yue.Internal.Type.MatchState (MatchState(..))
import Yue.Internal.Type.Parsable (class Parsable, parseParam)
import Yue.Internal.Type.Query (lookupQuery)
import Yue.Internal.Util (setResponseDefHeader, setResponseJson, setResponseText)

askRequest :: forall e m. Monad m => ActionT e m Request
askRequest = asks _.req

askResponse :: forall e m. Monad m => ActionT e m Response
askResponse = asks _.res

-- | 获取当前访问地址对象。
getURL :: forall e m. Monad m => ActionT e m URL
getURL = asks _.url

tryQuery :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryQuery key = do
  q <- asks _.query
  case lookupQuery key =<< q of
    Nothing -> pure Nothing
    Just v -> withExceptT (ActionInnerError <<< ActionQueryError) $ except $ parseParam v
query :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
query key = do
  mv <- tryQuery key
  case mv of
    Just v -> pure v
    Nothing -> throwError $ ActionInnerError $ ActionQueryError $ "query参数" <> key <> "未提供。"

tryParam :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam key = gets f
  where f (MatchState s) = Map.lookup key s.paramMap >>= hush <<< parseParam

param :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
param key = do
  (MatchState s) <- get
  case Map.lookup key s.paramMap of
    Just x -> withExceptT (ActionInnerError <<< ActionParamError) $ except $ parseParam x
    Nothing -> throwError $ ActionInnerError $ ActionParamError $ "param参数" <> key <> "未提供。"

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
