module Yue.Server.Param ( tryParam
                        , param
                        ) where

import Prelude

import Control.Monad.State.Trans (get)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Yue.Internal.Type.Action (ActionT, catchAction, exceptEither, throwChecked)
import Yue.Internal.Type.MatchState (MatchState(..))
import Yue.Internal.Type.Parsable (class Parsable, parseParam)

param :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
param key = do
  (MatchState s) <- get
  case Map.lookup key s.paramMap of
    Nothing -> throwChecked $ "param参数不存在" <> key
    Just a -> exceptEither $ parseParam a

tryParam :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam = catchAction <<< param
