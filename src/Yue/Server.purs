module Yue.Server ( runServer
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
import Node.Encoding (Encoding(..))
import Node.HTTP (createServer, listen, responseAsStream, setStatusCode)
import Node.Stream (end, writeString)
import Yue.Internal.Type.Action (ActionST(..), ActionT, mkActionEnv)
import Yue.Internal.Type.MatchState (initMatchState)
import Yue.Internal.Type.Path (mkRequestPath)
import Yue.Internal.Type.ResponseError (class IsResponseError, errorContent, errorStatus)
import Yue.Server.Config (ServerOption)

runServer :: forall e. IsResponseError e => ServerOption -> ActionT e Effect Unit -> Effect Unit -> Effect Unit
runServer { addr, port } action callback = do
  server <- createServer \req res -> do
    let env = mkActionEnv req res
        st = initMatchState $ mkRequestPath $ fromMaybe "" $ toMaybe env.url.pathname
    r <- flip evalStateT st $ flip runReaderT env $ runExceptT action
    case r of
      (Right o) -> pure o
      (Left e) -> case e of
        ActionFinish -> pure unit
        ActionError e' -> do
          let statusCode = errorStatus e'
              content = errorContent e'
              s = responseAsStream res
          setStatusCode res statusCode
          void $ writeString s UTF8 (stringify content) $ pure unit
          end s $ pure unit
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }
