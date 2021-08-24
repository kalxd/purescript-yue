module Yue.Server.Header ( tryHeader
                         , header
                         , header'
                         , setHeader
                         , setJsonHeader
                         , setCode
                         ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Maybe (Maybe)
import Data.String (toLower)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (lookup)
import Node.HTTP as HTTP
import Yue.Internal.Type.Action (ActionT, fromMaybeAction, throwCheckedMaybe)

header :: forall e m. Monad m => String -> ActionT e m String
header key = asks _.req >>= (throwCheckedMaybe msg <<< f)
  where msg = "请求头并没有" <> key <> "！"
        f = lookup (toLower key) <<< HTTP.requestHeaders

tryHeader :: forall e m. Monad m => String -> ActionT e m (Maybe String)
tryHeader key = do
  req <- asks _.req
  pure $ lookup (toLower key) $ HTTP.requestHeaders req

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
