module Yue.Server.Control ( finish
                          , throw
                          ) where

import Prelude

import Control.Monad.Except.Trans (throwError)
import Yue.Internal.Type.Action (ActionST(..), ActionT)

finish :: forall e m a. Monad m => ActionT e m a
finish = throwError ActionFinish

throw :: forall e m a. Monad m => e -> ActionT e m a
throw = throwError <<< ActionError
