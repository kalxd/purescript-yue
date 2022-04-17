# Yue

名曰“月”，月洒大地，不像红日烈焰直曝，恰似那流水轻柔婉转。

# 一点小声明

写该框架出于现实状况：PureScript后端积累过少。
虽说不少人写PureScript，基本上用在前端方向，前端框架我就见过不少，后端的网络框架寥寥无几，Payload看起来十分不错，就是类型过于安全，写起来有些繁琐，其他原生语言实现的框架就更少了，基本上都基于npm包装。没办法，只能自己动手写框架。
写到后面才发现PureScript在后端这一块，生态甚是惨淡。想想也是，基础框架尚且如此，更遑论数据库、日志相关工具。

后来，我终于换电脑了，不再使用难用的windows，编程轻松了不少，直接上Haskell，不用再写PureScript。

按说这个库没有任何使用价值理应删除，但还是想保留下来以示纪念，也不枉我在小方山苦苦熬了两月余。

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
