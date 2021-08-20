module Yue.Server.Body ( tryJson
                       , json
                       , setText
                       , setJson
                       ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, parseJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.HTTP (Response)
import Yue.Internal.Type.Action (ActionT, catchAction, exceptEither)
import Yue.Internal.Util (getRequestBodyText, setResponseDefHeader, setResponseJson, setResponseText)
import Yue.Server.Control (finish)

askRes :: forall e m. Monad m => ActionT e m Response
askRes = asks _.res

json :: forall e a. DecodeJson a => ActionT e Aff a
json = do
  req <- asks _.req
  bodystr <- liftAff $ getRequestBodyText req
  exceptEither $ decodeJson =<< parseJson bodystr

tryJson :: forall e. ActionT e Aff (Maybe String)
tryJson = catchAction json

-- | 设置响应体。
setText :: forall e m. MonadEffect m => String -> ActionT e m Unit
setText text = do
  res <- askRes
  liftEffect $ setResponseText res text
  finish

-- | 设置响应体。
setJson :: forall e m a. EncodeJson a => MonadEffect m => a -> ActionT e m Unit
setJson a = do
  res <- askRes
  liftEffect do
    setResponseDefHeader res
    setResponseJson res a
  finish
