module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Error (AppError)
import Yue.Server (runServer)
import Yue.Server.Action (setText, throwE, tryParam)
import Yue.Server.Router (route)

simpleApplication :: ActionT (AppError String) Effect Unit
simpleApplication = do
  route "/item/:id" do
    route "/a/:a" do
      a <- tryParam "a"
      when (a == Just 1) $ throwE "sb"
      setText $ show $ fromMaybe 1 a
  route "/pro/:id/i/:addr" do
    id <- tryParam "addr"
    setText $ fromMaybe "什么都没有" $ id

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer option simpleApplication do
    log "server starting!"
