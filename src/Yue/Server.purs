module Yue.Server ( runServer
                  , Application
                  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Trans (evalStateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Node.HTTP (Request, Response, createServer, listen)
import Yue.Internal.Type.Action (ActionT, mkActionEnv)
import Yue.Internal.Type.MatchState (initMatchState)
import Yue.Server.Config (ServerOption)

type Application = Request -> Response -> Effect Unit

runServer :: ServerOption -> ActionT Effect Unit -> Effect Unit -> Effect Unit
runServer { addr, port } action callback = do
  server <- createServer \req res -> do
    let env = mkActionEnv req res
        st = initMatchState $ fromMaybe "" $ toMaybe env.url.pathname
    r <- flip evalStateT st $ flip runReaderT env $ runExceptT action
    case r of
      (Right o) -> pure o
      (Left _) -> pure unit
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }
