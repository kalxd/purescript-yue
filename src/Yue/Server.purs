module Yue.Server ( ActionM
                  , module S
                  , module R
                  , module E
                  ) where

import Effect.Aff (Aff)
import Yue.Internal.Type.Action (ActionT)
import Yue.Internal.Type.Error (AppError)
import Yue.Server.Body (json, json', setJson, setText, tryJson) as E
import Yue.Server.Control (finish, throw, throwE) as E
import Yue.Server.Header (header, header', setCode, setHeader, setJsonHeader, tryHeader) as E
import Yue.Server.Param (tryParam, param, param') as E
import Yue.Server.Query (query, query', queryList, queryList', tryQuery, tryQueryList) as E
import Yue.Server.Router (route, get, post, patch, put, delete) as R
import Yue.Server.Run (runServer) as S

-- | 一个简单的别名，也许没什么用处。
type ActionM e = ActionT (AppError e) Aff
