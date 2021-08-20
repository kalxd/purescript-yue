module Yue.Server.Run ( runServer
                      ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Trans (evalStateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.HTTP (Request, Response, createServer, listen, setStatusCode)
import Yue.Internal.Type.Action (ActionST(..), ActionT, mkActionEnv)
import Yue.Internal.Type.MatchState (initMatchState)
import Yue.Internal.Type.Path (mkRequestPath)
import Yue.Internal.Type.ResponseError (class IsResponseError, errorContent, errorStatus)
import Yue.Internal.Util (setResponseDefHeader, setResponseJson)
import Yue.Server.Config (ServerOption)

setResponseError :: forall e. IsResponseError e => Response -> e -> Effect Unit
setResponseError res e = do
  let code = errorStatus e
      msg = errorContent e
  setStatusCode res code
  setResponseDefHeader res
  setResponseJson res msg

sendActionError :: forall e. IsResponseError e => Response -> ActionST e -> Effect Unit
sendActionError _ ActionFinish = pure unit
sendActionError res (ActionChecked e) = setResponseError res e
sendActionError res (ActionError e) = setResponseError res e

wrapApplication :: forall e. IsResponseError e
                   => ActionT e Aff Unit
                   -> Request
                   -> Response
                   -> Effect Unit
wrapApplication action req res = launchAff_ aff
  where env = mkActionEnv req res
        st = initMatchState $ mkRequestPath $ fromMaybe "" $ toMaybe env.url.pathname
        aff = do
          a <- flip evalStateT st $ flip runReaderT env $ runExceptT action
          case a of
            (Right _) -> pure unit
            (Left e) -> liftEffect $ sendActionError res e

runServer :: forall e. IsResponseError e
             => ServerOption
             -> ActionT e Aff Unit
             -> Effect Unit
             -> Effect Unit
runServer { addr, port } action callback = do
  server <- createServer $ wrapApplication action
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }
