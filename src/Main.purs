module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.HTTP (requestURL, responseAsStream)
import Node.Stream (end, writeString)
import Yue.Server (Application, runServer)

simpleApplication :: Application
simpleApplication req res = do
  let msg = requestURL req
      output = responseAsStream res
  void $ writeString output UTF8 msg (pure unit)
  end output (pure unit)

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer option simpleApplication do
    log "server starting!"
