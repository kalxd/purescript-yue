module Yue.Server.Action ( ActionRequest
                         , ActionT
                         , ActionE
                         ) where

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Node.HTTP (Request, Response)

-- 一条请求状态。
data ActionE = ActionFinish
             | ActionNext

-- | 一个请求携带的全部信息，同时解析出一些常用、必要原始信息，以供后续使用。
type ActionRequest = { req :: Request
                     , res :: Response
                     }

type ActionT :: forall k. (Type -> Type) -> k -> Type -> Type
type ActionT m a = ExceptT ActionE (ReaderT ActionRequest m)
