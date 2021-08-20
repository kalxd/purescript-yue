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

header :: forall e m. Monad m => String -> ActionT e m String
header key = asks _.req >>= (throwCheckedMaybe msg <<< f)
  where msg = "请求头并没有" <> key <> "！"
        f = lookup (toLower key) <<< HTTP.requestHeaders

tryHeader :: forall e m. Monad m => String -> ActionT e m (Maybe String)
tryHeader key = do
  req <- asks _.req
  pure $ lookup (toLower key) $ HTTP.requestHeaders req

setHeader :: forall e m. MonadEffect m => String -> String -> ActionT e m Unit
setHeader key value = do
  res <- asks _.res
  liftEffect $ HTTP.setHeader res key value
