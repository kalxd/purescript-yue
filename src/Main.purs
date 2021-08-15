module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Yue.Server (runServer')
import Yue.Server.Action (ActionT, setText)

simpleApplication :: ActionT (Effect Unit)
simpleApplication = do
  setText "hello world"

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer' option simpleApplication do
    log "server starting!"
