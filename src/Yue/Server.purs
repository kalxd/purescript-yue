module Yue.Server ( runServer
                  , Application
                  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.HTTP (Request, Response, createServer, listen)
import Yue.Config (ServerOption)

type Application = Request -> Response -> Effect Unit

runServer :: ServerOption -> Application -> Effect Unit -> Effect Unit
runServer { addr, port } application callback = do
  server <- createServer application
  listen server option callback
  where option = { backlog: Nothing
                 , hostname: addr
                 , port
                 }
