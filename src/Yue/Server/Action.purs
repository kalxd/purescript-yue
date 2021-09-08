module Yue.Server.Action (mapActionT) where

import Control.Monad.Except.Trans (mapExceptT)
import Control.Monad.Reader.Trans (mapReaderT)
import Control.Monad.State.Trans (mapStateT)
import Yue.Internal.Type.Action (ActionT)

mapActionT :: forall e m1 m2 a. (forall b. m1 b -> m2 b) -> ActionT e m1 a -> ActionT e m2 a
mapActionT f = mapExceptT g
  where g = mapReaderT h
        h = mapStateT f
