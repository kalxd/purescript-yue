module Yue.Server.Query where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Yue.Internal.Type.Action (ActionT, exceptEither, tryMapAction)
import Yue.Internal.Type.Parsable (class Parsable, parseParam)
import Yue.Internal.Type.Query (lookupQuery, mkQuery)

tryQuery :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryQuery key = do
  url <- asks _.url
  case lookupQuery key =<< mkQuery <$> toMaybe url.query of
    Nothing -> pure Nothing
    Just v -> exceptEither $ parseParam v

query :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
query key = tryMapAction msg $ tryQuery key
  where msg = "query不存在" <> key
