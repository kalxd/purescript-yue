module Yue.Server.Action ( getURL
                         , getURLString
                         , getQuery
                         , tryParam
                         , setText
                         , finish
                         , throw
                         ) where

import Prelude

import Control.Monad.Except.Trans (throwError)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.State.Trans (gets)
import Data.Either (hush)
import Data.HashMap as Map
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestURL, responseAsStream)
import Node.Stream (end, writeString)
import Node.URL (URL)
import Yue.Internal.Type.Action (ActionST(..), ActionT)
import Yue.Internal.Type.MatchState (MatchState(..))
import Yue.Internal.Type.Parsable (class IsParamParsable, parseParam)
import Yue.Internal.Type.Query (lookupQuery)

askRequest :: forall e m. Monad m => ActionT e m Request
askRequest = asks _.req

askResponse :: forall e m. Monad m => ActionT e m Response
askResponse = asks _.res

-- | 获取当前访问地址对象。
getURL :: forall e m. Monad m => ActionT e m URL
getURL = asks _.url

getQuery :: forall e m. Monad m => String -> ActionT e m (Maybe String)
getQuery key = do
  query <- asks _.query
  pure $ lookupQuery key =<< query

tryParam :: forall e m a. IsParamParsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam key = gets f
  where f (MatchState s) = Map.lookup key s.paramMap >>= hush <<< parseParam

-- | 获取当前访问原始地址。
getURLString :: forall e m. Monad m => ActionT e m String
getURLString = requestURL <$> askRequest

setText :: forall e m. MonadEffect m => String -> ActionT e m Unit
setText text = do
  res <- askResponse
  liftEffect do
    let s = responseAsStream res
    void $ writeString s UTF8 text (pure unit)
    end s $ pure unit
  finish

finish :: forall e m a. Monad m => ActionT e m a
finish = throwError ActionFinish

throw :: forall e m a. Monad m => e -> ActionT e m a
throw = throwError <<< ActionError
