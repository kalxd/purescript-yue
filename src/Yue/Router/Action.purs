module Yue.Router.Action ( ActionT
                         , ActionST
                         , query
                         ) where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks)
import Control.Monad.State.Trans (StateT)
import Data.Array (find)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd)
import Effect.Class (class MonadEffect)
import Node.HTTP as HTTP
import Node.Stream as Stream
import URI.Extra.QueryPairs as Q

type ActionST = { req :: HTTP.Request
                , res :: HTTP.Response
                , output :: Stream.Writable ()
                , query :: Q.QueryPairs Q.Key Q.Value
                }

-- | 单条请求的状态。
type ActionT s m a = ReaderT ActionST (StateT s m) a

query :: forall s m. MonadAsk ActionST m => MonadEffect m => String -> ActionT s m (Maybe String)
query key = do
  (Q.QueryPairs qs) <- asks _.query
  let f x = Q.keyFromString key == fst x
      value = Q.valueToString <$> (find f qs >>= snd)
  pure value
