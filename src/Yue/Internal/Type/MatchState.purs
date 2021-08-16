module Yue.Internal.Type.MatchState where

import Data.HashMap (HashMap, empty, insert)
import Data.String.NonEmpty as NEString
import Yue.Internal.Type.Path (RequestPath)

type ParamMap = HashMap String String

newtype MatchState = MatchState { path :: RequestPath
                                , paramMap :: ParamMap
                                }

initMatchState :: RequestPath -> MatchState
initMatchState path = MatchState { path, paramMap: empty }

insertParamMap :: NEString.NonEmptyString -> NEString.NonEmptyString -> MatchState -> MatchState
insertParamMap k v (MatchState s) = MatchState s { paramMap = map }
  where map = insert key value s.paramMap
        key = NEString.toString k
        value = NEString.toString v
