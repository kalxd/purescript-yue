module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Yue.Internal.Type.Action (ActionT)
import Yue.Server (runServer)
import Yue.Server.Action (getQuery, setText)
import Yue.Server.Router (route)

simpleApplication :: ActionT Effect Unit
simpleApplication = do
  route "/a" do
    a <- getQuery "a"
    setText $ fromMaybe "hello" a
  route "/b" do
    b <- getQuery "b"
    setText $ fromMaybe "b" b

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer option simpleApplication do
    log "server starting!"
