module Yue.Server.Router (route) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Path (matchRouterPath, mkRequestPath, toRouterPath)
import Yue.Server.Action (finish, getURL)

route :: forall m a. Monad m => String -> ActionT m a -> ActionT m Unit
route path action = do
  url <- getURL
  let requestPath = mkRequestPath $ fromMaybe "" $ toMaybe $ url.pathname
      routerPath = toRouterPath path
  case matchRouterPath requestPath routerPath of
    Just _ -> do
      void $ action
      finish
    Nothing -> pure unit
