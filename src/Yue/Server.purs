module Yue.Server ( runServer
                  , runServer'
                  , Application
                  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (runReader)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.HTTP (Request, Response, createServer, listen)
import Yue.Server.Action (ActionT, mkAction)
import Yue.Server.Config (ServerOption)

type Application = Request -> Response -> Effect Unit

runServer' :: ServerOption -> ActionT (Effect Unit) -> Effect Unit -> Effect Unit
runServer' { addr, port } action callback = do
  server <- createServer \req res -> do
    let env = mkAction req res
    case flip runReader env $ runExceptT action of
      (Right o) -> o
      (Left _) -> pure unit
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }


runServer :: ServerOption -> Application -> Effect Unit -> Effect Unit
runServer { addr, port } application callback = do
  server <- createServer application
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }
