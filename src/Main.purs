module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Error (AppError)
import Yue.Server (runServer)
import Yue.Server.Action (setText, tryParam)
import Yue.Server.Header (tryHeader)
import Yue.Server.Router (route)

simpleApplication :: ActionT (AppError String) Effect Unit
simpleApplication = do
  route "/item/:id" do
    route "/a/:a" do
      h <- tryHeader "Content-Type"
      setText $ fromMaybe "未知头部" h
  route "/pro/:id/i/:addr" do
    id <- tryParam "addr"
    setText $ fromMaybe "什么都没有" $ id

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer option simpleApplication do
    log "server starting!"
