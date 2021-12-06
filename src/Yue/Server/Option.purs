module Yue.Server.Option where

-- | 启动服务最基本信息，目前不支持*https*。
type ServerOption r = { addr :: String
                      , port :: Int
                      | r}

defServerOption :: ServerOption ()
defServerOption = { addr: "127.0.0.1"
                  , port: 3000
                  }
