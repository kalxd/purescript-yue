module Yue.Internal.Query where

import Control.Category ((<<<))
import Data.Maybe (Maybe)
import Foreign.Object (Object, lookup)
import Node.URL as URL
import Unsafe.Coerce (unsafeCoerce)

newtype Query a = Query (Object a)

mkQuery :: forall a. String -> Query a
mkQuery = Query <<< unsafeCoerce <<< URL.parseQueryString

lookupQuery :: forall a. String -> Query a -> Maybe a
lookupQuery key (Query o) = lookup key o
