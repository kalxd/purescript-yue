module Yue.Server.Run ( runServer
                      , runServerAt
                      ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Trans (evalStateT)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, createServer, listen, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, writeString)
import Yue.Internal.Type.Action (ActionST(..), ActionT, mkActionEnv)
import Yue.Internal.Type.MatchState (initMatchState)
import Yue.Internal.Type.Path (mkRequestPath)
import Yue.Internal.Type.ResponseError (class IsResponseError, errorContent, errorStatus)
import Yue.Server.Option (ServerOption)

setResponseError :: forall e. IsResponseError e => Response -> e -> Effect Unit
setResponseError res e = do
  let code = errorStatus e
      msg = stringify $ errorContent e
      w = responseAsStream res
  setStatusCode res code
  setHeader res "Content-Type" "application/json"
  void $ writeString w UTF8 msg $ pure unit
  end w $ pure unit

sendActionError :: forall e. IsResponseError e => Response -> ActionST e -> Effect Unit
sendActionError _ ActionFinish = pure unit
sendActionError res (ActionError e) = setResponseError res e

buildApplication :: forall e. IsResponseError e
                   => ActionT e Aff Unit
                   -> Request
                   -> Response
                   -> Effect Unit
buildApplication action req res = launchAff_ aff
  where env = mkActionEnv req res
        st = initMatchState $ mkRequestPath $ fromMaybe "" $ toMaybe env.url.pathname
        aff = do
          a <- flip evalStateT st $ flip runReaderT env $ runExceptT action
          case a of
            (Right _) -> pure unit
            (Left e) -> liftEffect $ sendActionError res e

runServer :: forall e r. IsResponseError e
             => ServerOption r
             -> ActionT e Aff Unit
             -> Effect Unit
             -> Effect Unit
runServer { addr, port } action callback = do
  server <- createServer $ buildApplication action
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }

runServerAt :: forall e. IsResponseError e
               => Int
               -> ActionT e Aff Unit
               -> Effect Unit
               -> Effect Unit
runServerAt port = runServer { addr: "127.0.0.1"
                             , port
                             }
