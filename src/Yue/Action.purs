module Yue.Action ( ActionT
                  , ActionEnv
                  , ActionE
                  , query
                  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Data.Array (find)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd)
import Node.HTTP as HTTP
import Node.Stream as Stream
import URI.Extra.QueryPairs as Q

type ActionEnv = { req :: HTTP.Request
                 , res :: HTTP.Response
                 , outputSteam :: Stream.Writable ()
                 , query :: Q.QueryPairs Q.Key Q.Value
                 }

data ActionE e = ActionFinish
               | ActionError e

-- | 单条请求的状态。
type ActionT e m a = ExceptT (ActionE e) (ReaderT ActionEnv m) a

query :: forall s m. Monad m => String -> ActionT s m (Maybe String)
query key = do
  (Q.QueryPairs qs) <- asks _.query
  let f x = Q.keyFromString key == fst x
      value = Q.valueToString <$> (find f qs >>= snd)
  pure value
