module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Error (AppError)
import Yue.Server (get, json, post, queryList, route, runServer, setJson, setText)
import Yue.Server.Router (notFound)

type User = { id :: Int
            , name :: String
            }

defUser :: User
defUser = { id: 1, name: "hello"}

simpleApplication :: ActionT (AppError String) Aff Unit
simpleApplication = do
  route "/item/" do
    get "/a/:id" do
      a :: Array Int <- queryList "id"
      setJson a
    post "/a" do
      user :: User <- json
      setJson user

  route "/menu" do
    get "/b" do
      setText "hello world"
  notFound

main :: Effect Unit
main = do
  let option = { addr: "127.0.0.1", port: 3000 }
  runServer option simpleApplication do
    log "server starting!"
