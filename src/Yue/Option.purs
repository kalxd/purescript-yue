module Yue.Config where

-- | 启动服务最基本信息，目前不支持*https*。
type ServerOption = { addr :: String
                    , port :: Int
                    }
