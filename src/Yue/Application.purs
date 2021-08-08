-- | 整个服务内部应用。
module Yue.Application ( ApplicationState
                       , ApplicationT
                       , setStatus
                       ) where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks)
import Control.Monad.Trans.Class (lift)
import Effect.Class (class MonadEffect, liftEffect)
import Node.HTTP (Request, Response)
import Node.HTTP as Http

-- | 一整条请求的状态。
type ApplicationState = { res :: Response
                        , req :: Request
                        }

type ApplicationT :: forall k. (k -> Type) -> k -> Type
type ApplicationT = ReaderT ApplicationState

-- askRequest :: forall m. MonadAsk ApplicationState m => ApplicationT m Request
-- askRequest = lift $ asks _.req

askResponse :: forall m. MonadAsk ApplicationState m => ApplicationT m Response
askResponse = lift $ asks _.res

-- | 设定*响应*状态码。
setStatus :: forall m. MonadEffect m
             => MonadAsk ApplicationState m
             => Int
             -> ApplicationT m Unit
setStatus code = do
  res <- askResponse
  liftEffect $ Http.setStatusCode res code
