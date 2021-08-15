module Yue.Server.Action ( ActionRequest
                         , ActionT
                         , ActionE
                         , mkAction
                         , setText
                         ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), throwError)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, responseAsStream)
import Node.Stream (end, writeString)

-- 一条请求状态。
data ActionE = ActionFinish
             | ActionError

-- | 一个请求携带的全部信息，同时解析出一些常用、必要原始信息，以供后续使用。
type ActionRequest = { req :: Request
                     , res :: Response
                     }

type ActionT m = ExceptT ActionE (ReaderT ActionRequest m)

finish :: forall m a. Monad m => ActionT m a
finish = throwError ActionFinish

mkAction :: Request -> Response -> ActionRequest
mkAction req res = { req, res }

askResponse :: forall m. Monad m => ActionT m Response
askResponse = asks _.res

setText :: forall m. MonadEffect m => String -> ActionT m Unit
setText text = do
  res <- askResponse
  liftEffect do
    let s = responseAsStream res
    void $ writeString s UTF8 text (pure unit)
    end s $ pure unit
  finish
