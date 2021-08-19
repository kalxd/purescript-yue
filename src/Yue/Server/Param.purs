module Yue.Server.Param ( tryParam
                        , param
                        ) where

import Prelude

import Control.Monad.Except.Trans (except, withExceptT)
import Control.Monad.State.Trans (get, gets)
import Data.Either (hush)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Yue.Internal.Type.Action (ActionST(..), ActionT, throwChecked)
import Yue.Internal.Type.Error (YueError(..))
import Yue.Internal.Type.MatchState (MatchState(..))
import Yue.Internal.Type.Parsable (class Parsable, parseParam)

tryParam :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam key = gets f
  where f (MatchState s) = Map.lookup key s.paramMap >>= hush <<< parseParam

param :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
param key = do
  (MatchState s) <- get
  case Map.lookup key s.paramMap of
    Just x -> withExceptT (ActionChecked <<< YueError) $ except $ parseParam x
    Nothing -> throwChecked $ "param参数" <> key <> "未提供。"
