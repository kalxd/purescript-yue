module Yue.Server.Action ( ActionRequest
                         , ActionT
                         , ActionE
                         , mkAction
                         , setText
                         ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader (Reader)
import Control.Monad.Reader.Trans (asks)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, responseAsStream)
import Node.Stream (end, writeString)

-- 一条请求状态。
data ActionE = ActionFinish
             | ActionNext

-- | 一个请求携带的全部信息，同时解析出一些常用、必要原始信息，以供后续使用。
type ActionRequest = { req :: Request
                     , res :: Response
                     }

type ActionT = ExceptT ActionE (Reader ActionRequest)

mkAction :: Request -> Response -> ActionRequest
mkAction req res = { req, res }

askResponse :: ActionT Response
askResponse = asks _.res

setText :: forall m. MonadEffect m => String -> ActionT (m Unit)
setText text = do
  res <- askResponse
  pure $ liftEffect do
    let s = responseAsStream res
    void $ writeString s UTF8 text (pure unit)
    end s $ pure unit
