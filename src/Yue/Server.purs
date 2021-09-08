module Yue.Server ( ActionM
                  , module S
                  , module R
                  , module E
                  , module Aff
                  ) where

import Yue.Internal.Type.Error (AppError)

import Effect.Aff (Aff) as Aff
import Effect.Aff.Class (class MonadAff, liftAff) as Aff
import Yue.Internal.Type.Action (ActionT) as E

import Yue.Server.Body (json, json', setJson, setText, tryJson) as E
import Yue.Server.Action (mapActionT) as E
import Yue.Server.Control (finish, throw, throwE) as E
import Yue.Server.Header (header, header', setCode, setHeader, setJsonHeader, tryHeader) as E
import Yue.Server.Param (tryParam, param, param') as E
import Yue.Server.Query (query, query', queryList, queryList', tryQuery, tryQueryList) as E
import Yue.Server.Router (delete, get, notFound, patch, post, put, route) as R
import Yue.Server.Run (runServer) as S
import Yue.Server.Option (ServerOption) as S

-- | 一个简单的别名，也许没什么用处。
type ActionM e = E.ActionT (AppError e) Aff.Aff
