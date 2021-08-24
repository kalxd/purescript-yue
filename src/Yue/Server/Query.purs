module Yue.Server.Query ( queryList
                        , tryQueryList
                        , queryList'
                        , query
                        , tryQuery
                        , query'
                        ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import Yue.Internal.Type.Action (ActionT, catchAction, exceptEither, fromMaybeAction, throwChecked, throwCheckedMaybe)
import Yue.Internal.Type.Parsable (class Parsable, parseParam)
import Yue.Internal.Type.Query (Query, lookupQuery, mkQuery)

askQuery :: forall e m. Monad m => ActionT e m Query
askQuery = mkQuery <$> (throwCheckedMaybe msg =<< toMaybe <$> asks _.url.query)
  where msg = "query参数为空！"

queryList :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Array a)
queryList key = do
  q <- askQuery
  case lookupQuery key q of
    Nothing -> throwChecked "query为空！"
    Just xs -> exceptEither $ traverse parseParam xs

tryQueryList :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe (Array a))
tryQueryList = catchAction <<< queryList

queryList' :: forall e m a. Parsable a => Monad m => Array a -> String -> ActionT e m (Array a)
queryList' def key = fromMaybeAction (tryQueryList key) def

query :: forall e m a. Parsable a => Monad m => String -> ActionT e m a
query key = do
  xs <- tryQueryList key
  case head =<< xs of
    Nothing -> throwChecked $ "query参数不存在" <> key <> "！"
    Just x -> exceptEither $ parseParam x

tryQuery :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryQuery = catchAction <<< query

query' :: forall e m a. Parsable a => Monad m => a -> String -> ActionT e m a
query' def key = fromMaybeAction (tryQuery key) def
