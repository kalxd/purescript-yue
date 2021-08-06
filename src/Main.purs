module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.HTTP (ListenOptions, Request, Response, createServer, listen, requestURL, responseAsStream, setStatusCode)
import Node.Stream (end, writeString)

type Application = Request -> Response -> Effect Unit

simpleApplication :: Application
simpleApplication req res = do
  let msg = requestURL req
      output = responseAsStream res
  setStatusCode res 200
  void $ writeString output UTF8 msg (pure unit)
  end output (pure unit)

serverListen :: ListenOptions
serverListen = { backlog: Nothing
               , hostname: "127.0.0.1"
               , port: 3000
               }

main :: Effect Unit
main = do
  server <- createServer simpleApplication
  listen server serverListen do
    log "server starting!"
