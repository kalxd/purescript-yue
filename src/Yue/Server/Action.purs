module Yue.Server.Action ( getURL
                         , getURLString
                         , getQuery
                         , setText
                         , finish
                         ) where

import Prelude

import Control.Monad.Except.Trans (throwError)
import Control.Monad.Reader.Trans (asks)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestURL, responseAsStream)
import Node.Stream (end, writeString)
import Node.URL (URL)
import Yue.Internal.Type.Action (ActionST(..), ActionT)
import Yue.Internal.Type.Query (lookupQuery)

finish :: forall m a. Monad m => ActionT m a
finish = throwError ActionFinish

askRequest :: forall m. Monad m => ActionT m Request
askRequest = asks _.req

askResponse :: forall m. Monad m => ActionT m Response
askResponse = asks _.res

-- | 获取当前访问地址对象。
getURL :: forall m. Monad m => ActionT m URL
getURL = asks _.url

getQuery :: forall m. Monad m => String -> ActionT m (Maybe String)
getQuery key = do
  query <- asks _.query
  pure $ lookupQuery key =<< query

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
