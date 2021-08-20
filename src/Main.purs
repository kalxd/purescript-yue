module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Error (AppError)
import Yue.Server (runServer, setText, tryParam)
import Yue.Server.Body (json, setJson)
import Yue.Server.Router (route)

type User = { id :: Int
            , name :: String
            }

simpleApplication :: ActionT (AppError String) Aff Unit
simpleApplication = do
  route "/item/:id" do
    route "/a/:a" do
      user :: User <- json
      setJson user
  route "/pro/:id/i/:addr" do
    id <- tryParam "addr"
    setText $ fromMaybe "什么都没有" $ id

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer option simpleApplication do
    log "server starting!"
