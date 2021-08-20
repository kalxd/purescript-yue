module Yue.Server.Query where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Yue.Internal.Type.Action (ActionT, catchAction, exceptEither, throwChecked)
import Yue.Internal.Type.Parsable (class Parsable, parseParam)
import Yue.Internal.Type.Query (lookupQuery, mkQuery)

query :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
query key = do
  url <- asks _.url
  case lookupQuery key =<< mkQuery <$> toMaybe url.query of
    Nothing -> throwChecked $ "query参数不存在" <> key
    Just v -> exceptEither $ parseParam v

tryQuery :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryQuery = catchAction <<< query
