module Yue.Internal.Type.MatchState where

import Prelude

import Data.List (uncons)
import Data.HashMap (HashMap, empty, insert)
import Data.Maybe (Maybe)
import Data.String.NonEmpty as NEString
import Yue.Internal.Type.Path (RequestPath(..))

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

-- | 弹出第一个路径。
unconsPath :: MatchState -> Maybe { head :: NEString.NonEmptyString , rest :: MatchState }
unconsPath (MatchState s) = do
  let RequestPath path = s.path
  p <- uncons path
  pure { head: p.head
       , rest: MatchState $ s { path = RequestPath p.tail }
       }
