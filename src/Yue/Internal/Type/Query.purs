module Yue.Internal.Type.Query where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign, isArray, typeOf, unsafeFromForeign)
import Foreign.Object (Object, lookup)
import Node.URL as URL
import Unsafe.Coerce (unsafeCoerce)

newtype Query = Query (Object Foreign)

mkQuery :: String -> Query
mkQuery = Query <<< unsafeCoerce <<< URL.parseQueryString

fmtValue :: Foreign -> Maybe (Array String)
fmtValue v | isArray v = Just $ unsafeFromForeign v
           | typeOf v == "string" = Just $ [unsafeFromForeign v]
           | otherwise = Nothing

lookupQuery :: String -> Query -> Maybe (Array String)
lookupQuery key (Query o) = lookup key o >>= fmtValue
