# Yue

名曰“月”，月洒大地，如丝如缕，丝丝缕缕又织“月”。

# 教程

## 最简服务 ##

```purescript
module Main (main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Yue.Server (ActionM, ServerOption, get, param, runServer, setJson)

type Application = ActionM String

-- 整个服务路由的定义。
application :: Application Unit
application = do
  get "/item/:id" do
    id :: String <- param "id"
    setJson id

option :: ServerOption
option = { addr: "127.0.0.1"
         , port: 3000
         }

main :: Effect Unit
main = runServer option application $ log "服务启动了！"
```

## 多条路由 ##

```purescript
application :: Application Unit
application = do
  get "/item/:id" do
    id :: String <- param "id"
    setJson id

  route "/user" do
    post "/login" do
      setJson "login successed"
    get "/:id" do
      id <- param "id"
      setJson $ "user" <> id
```


# 发布协议 #

AGPL v3
