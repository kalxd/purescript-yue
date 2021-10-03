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
import Yue.Internal.Util (mapLeft)

data ParamError = ParamNotFound String
                | ParamParseFail String

derive instance Eq ParamError

instance Show ParamError where
  show (ParamNotFound msg) = show msg
  show (ParamParseFail msg) = show msg

paramNotFound :: forall a. String -> Either ParamError a
paramNotFound key = Left $ ParamNotFound $ "param不存在" <> key <> "字段！"

fmtParamParseFail :: String -> String -> ParamError
fmtParamParseFail key msg = ParamParseFail $ "param解析" <> key <> "字段失败：" <> msg <> "。"

param :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Either ParamError a)
param key = do
  MatchState s <- get
  pure case Map.lookup key s.paramMap of
    Nothing -> paramNotFound key
    Just a -> mapLeft (fmtParamParseFail key) $ parseParam a

tryParam :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryParam key = hush <$> param key

param' :: forall e m a. Parsable a => Monad m => a -> String -> ActionT e m a
param' a p = fromMaybeAction (tryParam p) a
