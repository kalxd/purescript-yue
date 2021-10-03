module Yue.Server.Param ( tryParam
                        , param
                        , param'
                        , ParamError(..)
                        ) where

import Prelude

import Control.Monad.State.Trans (get)
import Data.Either (Either(..), hush)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Yue.Internal.Type.Action (ActionT, fromMaybeAction)
import Yue.Internal.Type.MatchState (MatchState(..))
import Yue.Internal.Type.Parsable (class Parsable, parseParam)

data ParamError = ParamNotFound String
                | ParamParseFail String

derive instance Eq ParamError

instance Show ParamError where
  show (ParamNotFound msg) = show msg
  show (ParamParseFail msg) = show msg

param :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Either ParamError a)
param key = do
  MatchState s <- get
  pure case Map.lookup key s.paramMap of
    Nothing -> Left $ ParamNotFound $ "param参数不存在" <> key <> "字段！"
    Just a -> case parseParam a of
      Right a' -> Right a'
      Left e -> Left $ ParamParseFail e

tryParam :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam key = hush <$> param key

param' :: forall e m a. Parsable a => Monad m => a -> String -> ActionT e m a
param' a p = fromMaybeAction (tryParam p) a
