module Yue.Server.Header ( tryHeader
                         , header
                         , setHeader
                         ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Maybe (Maybe)
import Data.String (toLower)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (lookup)
import Node.HTTP as HTTP
import Yue.Internal.Type.Action (ActionT, throwCheckedMaybe)

tryHeader :: forall e m. Monad m => String -> ActionT e m (Maybe String)
tryHeader key = do
  req <- asks _.req
  pure $ lookup (toLower key) $ HTTP.requestHeaders req

header :: forall e m. Monad m => String -> ActionT e m String
header key = throwCheckedMaybe msg =<< tryHeader key
  where msg = "请求头没有" <> key

setHeader :: forall e m. MonadEffect m => String -> String -> ActionT e m Unit
setHeader key value = do
  res <- asks _.res
  liftEffect $ HTTP.setHeader res key value
