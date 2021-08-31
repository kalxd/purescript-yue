module Yue.Server.Run ( runServer
                      , runServerT
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
sendActionError res (ActionChecked e) = setResponseError res e
sendActionError res (ActionError e) = setResponseError res e

runServer :: forall e. IsResponseError e
             => ServerOption
             -> ActionT e Aff Unit
             -> Effect Unit
             -> Effect Unit
runServer = runServerT identity

buildApplication :: forall e m a. IsResponseError e
                    => Monad m
                    => (m (Either (ActionST e) a) -> Aff (Either (ActionST e) a))
                    -> ActionT e m a
                    -> Request
                    -> Response
                    -> Effect Unit
buildApplication trans action req res = launchAff_ aff
  where env = mkActionEnv req res
        st = initMatchState $ mkRequestPath $ fromMaybe "" $ toMaybe env.url.pathname
        ma = flip evalStateT st $ flip runReaderT env $ runExceptT action
        aff = do
          a <- trans ma
          case a of
            (Right _) -> pure unit
            (Left e) -> liftEffect $ sendActionError res e

runServerT :: forall e m a. IsResponseError e
              => Monad m
              => (m (Either (ActionST e) a) -> Aff (Either (ActionST e ) a))
              -> ServerOption
              -> ActionT e m a
              -> Effect Unit
              -> Effect Unit
runServerT trans option action callback = do
  server <- createServer $ buildApplication trans action
  let option' = { backlog: Nothing
                , hostname: option.addr
                , port: option.port
                }
  listen server option' callback
