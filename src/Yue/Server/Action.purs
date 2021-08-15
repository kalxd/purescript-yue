module Yue.Server.Action ( ActionRequest
                         , ActionT
                         , ActionE
                         , mkAction
                         , getURL
                         , getURLString
                         , getQuery
                         , setText
                         , finish
                         ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestURL, responseAsStream)
import Node.Stream (end, writeString)
import Node.URL as URL
import Yue.Internal.Query as Q

-- 一条请求状态。
data ActionE = ActionFinish
             | ActionError

-- | 一个请求携带的全部信息，同时解析出一些常用、必要原始信息，以供后续使用。
type ActionRequest = { req :: Request
                     , res :: Response
                     , url :: URL.URL
                     , query :: Maybe (Q.Query String)
                     }

type ActionT m = ExceptT ActionE (ReaderT ActionRequest m)

-- | 立马中断处理流程，跳出整个流程。
finish :: forall m a. Monad m => ActionT m a
finish = throwError ActionFinish

mkAction :: Request -> Response -> ActionRequest
mkAction req res = { req, res, url, query }
  where url = URL.parse $ requestURL req
        query = Q.mkQuery <$> toMaybe url.query

askRequest :: forall m. Monad m => ActionT m Request
askRequest = asks _.req

askResponse :: forall m. Monad m => ActionT m Response
askResponse = asks _.res

-- | 获取当前访问地址对象。
getURL :: forall m. Monad m => ActionT m URL.URL
getURL = asks _.url

getQuery :: forall m. Monad m => String -> ActionT m (Maybe String)
getQuery key = do
  query <- asks _.query
  pure $ Q.lookupQuery key =<< query

-- | 获取当前访问原始地址。
getURLString :: forall m. Monad m => ActionT m String
getURLString = requestURL <$> askRequest

setText :: forall m. MonadEffect m => String -> ActionT m Unit
setText text = do
  res <- askResponse
  liftEffect do
    let s = responseAsStream res
    void $ writeString s UTF8 text (pure unit)
    end s $ pure unit
  finish
