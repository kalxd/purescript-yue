module Yue.Server ( runServer
                  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Trans (evalStateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Node.HTTP (Response, createServer, listen, setStatusCode)
import Yue.Internal.Type.Action (ActionST(..), ActionT, mkActionEnv)
import Yue.Internal.Type.MatchState (initMatchState)
import Yue.Internal.Type.Path (mkRequestPath)
import Yue.Internal.Type.ResponseError (class IsResponseError, errorContent, errorStatus)
import Yue.Internal.Util (setResponseDefHeader, setResponseJson)
import Yue.Server.Config (ServerOption)

sendError :: forall e. IsResponseError e => Response -> ActionST e -> Effect Unit
sendError _ ActionFinish = pure unit
sendError res (ActionError e) = do
  let code = errorStatus e
      msg = errorContent e
  setStatusCode res code
  setResponseDefHeader res
  setResponseJson res msg

runServer :: forall e. IsResponseError e => ServerOption -> ActionT e Effect Unit -> Effect Unit -> Effect Unit
runServer { addr, port } action callback = do
  server <- createServer \req res -> do
    let env = mkActionEnv req res
        st = initMatchState $ mkRequestPath $ fromMaybe "" $ toMaybe env.url.pathname
    r <- flip evalStateT st $ flip runReaderT env $ runExceptT action
    case r of
      (Right o) -> pure o
      (Left e) -> sendError res e
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }
