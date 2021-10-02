module Yue.Server.Body ( tryJson
                       , json
                       , json'
                       , setText
                       , setJson
                       ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, parseJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, requestAsStream, responseAsStream)
import Node.Stream as S
import Yue.Internal.Type.Action (ActionT, catchAction, exceptEither, fromMaybeAction)
import Yue.Internal.Util (parseJSON)
import Yue.Server.Control (finish)
import Yue.Server.Header (setJsonHeader)

askRes :: forall e m. Monad m => ActionT e m Response
askRes = asks _.res

getRequestBodyText :: Request -> Aff String
getRequestBodyText req = makeAff f
  where r = requestAsStream req
        f k = do
          buffer <- Ref.new ""
          let ok s = Ref.modify_ (_ <> s) buffer
              no = k <<< Left
              finish = Ref.read buffer >>= k <<< Right
          S.onDataString r UTF8 ok
          S.onEnd r finish
          S.onError r no
          pure nonCanceler

setBodyText :: Response -> String -> Effect Unit
setBodyText res s = do
  let r = responseAsStream res
  void $ S.writeString r UTF8 s $ pure unit
  S.end r $ pure unit

json :: forall e m a. MonadAff m => DecodeJson a => ActionT e m a
json = do
  req <- asks _.req
  bodystr <- liftAff $ getRequestBodyText req
  exceptEither $ decodeJson =<< parseJson bodystr

tryJson :: forall e m a. MonadAff m =>  DecodeJson a => ActionT e m (Maybe a)
tryJson = catchAction json

json' :: forall e m a. MonadAff m => DecodeJson a => a -> ActionT e m a
json' = fromMaybeAction tryJson

-- | 设置响应体。
setText :: forall e m. MonadEffect m => String -> ActionT e m Unit
setText text = do
  res <- askRes
  liftEffect $ setBodyText res text
  finish

-- | 设置响应体。
setJson :: forall e m a. EncodeJson a => MonadEffect m => a -> ActionT e m Unit
setJson a = do
  setJsonHeader
  setText $ parseJSON a
