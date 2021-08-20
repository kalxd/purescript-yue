module Yue.Server.Body where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Argonaut.Encode (class EncodeJson)
import Effect.Class (class MonadEffect, liftEffect)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Util (setResponseDefHeader, setResponseJson, setResponseText)
import Yue.Server.Control (finish)

-- | 设置响应体。
setText :: forall e m. MonadEffect m => String -> ActionT e m Unit
setText text = do
  res <- asks _.res
  liftEffect $ setResponseText res text
  finish

-- | 设置响应体。
setJson :: forall e m a. EncodeJson a => MonadEffect m => a -> ActionT e m Unit
setJson a = do
  res <- asks _.res
  liftEffect do
    setResponseDefHeader res
    setResponseJson res a
  finish
