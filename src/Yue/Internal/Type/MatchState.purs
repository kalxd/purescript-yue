module Yue.Internal.Type.MatchState where

import Data.HashMap (HashMap, empty)
import Yue.Internal.Type.Path (RequestPath, mkRequestPath)

type ParamMap = HashMap String String

newtype MatchState = MatchState { restPath :: RequestPath
                                , paramMap :: ParamMap
                                }

initMatchState :: String -> MatchState
initMatchState s = MatchState { restPath, paramMap }
  where restPath = mkRequestPath s
        paramMap = empty
