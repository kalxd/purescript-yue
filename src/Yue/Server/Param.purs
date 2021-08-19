module Yue.Server.Param ( tryParam
                        , param
                        ) where

import Prelude

import Control.Monad.State.Trans (get)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Yue.Internal.Type.Action (ActionT, exceptEither, tryMapAction)
import Yue.Internal.Type.MatchState (MatchState(..))
import Yue.Internal.Type.Parsable (class Parsable, parseParam)

tryParam :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam key = do
  (MatchState s) <- get
  case Map.lookup key s.paramMap of
    Just v -> exceptEither $ parseParam v
    Nothing -> pure Nothing

param :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
param key = tryMapAction msg (tryParam key)
  where msg = "param不存在" <> key
