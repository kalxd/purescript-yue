module Yue.Server.Control where

import Prelude

import Control.Monad.Except.Trans (throwError)
import Yue.Internal.Type.Action (ActionST(..), ActionT)
import Yue.Internal.Type.Error (AppError(..))

finish :: forall e m a. Monad m => ActionT e m a
finish = throwError ActionFinish

throw :: forall e m a. Monad m => e -> ActionT e m a
throw = throwError <<< ActionError

-- | 特供版，抛出的错误自动包装在`AppError`。
throwE :: forall e m a. Monad m => e -> ActionT (AppError e) m a
throwE = throw <<< AppError
