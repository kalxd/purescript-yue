module Yue.Internal.Type.Query where

import Prelude

import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object, lookup)
import Node.URL as URL
import Unsafe.Coerce (unsafeCoerce)

newtype Query = Query (Object Foreign)

mkQuery :: String -> Query
mkQuery = Query <<< unsafeCoerce <<< URL.parseQueryString

lookupQuery :: String -> Query -> Maybe (Array String)
lookupQuery key (Query o) = unsafeFromForeign <$> lookup key o
