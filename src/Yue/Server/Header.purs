module Yue.Server.Header ( tryHeader
                         , header
                         , header'
                         , setHeader
                         , setJsonHeader
                         , setCode
                         , HeaderError(..)
                         ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Either (Either, note')
import Data.Maybe (Maybe)
import Data.String (toLower)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (lookup)
import Node.HTTP as HTTP
import Yue.Internal.Type.Action (ActionT, fromMaybeAction)

newtype HeaderError = HeaderNotFound String

derive instance Eq HeaderError

instance Show HeaderError where
  show (HeaderNotFound msg) = show msg

tryHeader :: forall e m. Monad m => String -> ActionT e m (Maybe String)
tryHeader key = do
  req <- asks _.req
  pure $ lookup (toLower key) $ HTTP.requestHeaders req

header :: forall e m. Monad m => String -> ActionT e m (Either HeaderError String)
header key = note' f <$> tryHeader key
  where f _ = HeaderNotFound $ "请求头没有" <> key <> "字段！"

header' :: forall e m. Monad m => String -> String -> ActionT e m String
header' def key = fromMaybeAction (tryHeader key) def

setHeader :: forall e m. MonadEffect m => String -> String -> ActionT e m Unit
setHeader key value = do
  res <- asks _.res
  liftEffect $ HTTP.setHeader res key value

setJsonHeader :: forall e m. MonadEffect m => ActionT e m Unit
setJsonHeader = setHeader "Content-Type" "application/json"

setCode :: forall e m. MonadEffect m => Int -> ActionT e m Unit
setCode code = do
  res <- asks _.res
  liftEffect $ HTTP.setStatusCode res code
