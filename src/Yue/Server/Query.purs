module Yue.Server.Query ( queryList
                        , tryQueryList
                        , queryList'
                        , query
                        , tryQuery
                        , query'
                        , QueryError(..)
                        ) where

import Prelude

import Control.Monad.Reader.Trans (asks)
import Data.Array (head)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import Yue.Internal.Type.Action (ActionT, fromMaybeAction)
import Yue.Internal.Type.Parsable (class Parsable, parseParam)
import Yue.Internal.Type.Query (Query, lookupQuery, mkQuery)
import Yue.Internal.Util (mapLeft)

data QueryError = QueryNotFound String
                | QueryParseFail String

derive instance Eq QueryError

instance Show QueryError where
  show (QueryNotFound msg) = show msg
  show (QueryParseFail msg) = show msg

queryNotFound :: forall a. String -> Either QueryError a
queryNotFound = Left <<< QueryNotFound

askQuery :: forall e m. Monad m => ActionT e m (Either QueryError Query)
askQuery = f <$> toMaybe <$> asks _.url.query
  where f Nothing = queryNotFound "query参数为空！"
        f (Just a) = Right $ mkQuery a

queryList :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Either QueryError (Array a))
queryList key = (join <<< map f) <$> askQuery
  where f q = case lookupQuery key q of
          Nothing -> queryNotFound $ "query没有" <> key <> "字段！"
          Just xs -> mapLeft QueryParseFail $ traverse parseParam xs

tryQueryList :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe (Array a))
tryQueryList key = hush <$> queryList key

queryList' :: forall e m a. Parsable a => Monad m => Array a -> String -> ActionT e m (Array a)
queryList' def key = fromMaybeAction (tryQueryList key) def

query :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Either QueryError a)
query key = do
  xs <- queryList key
  pure do
    x <- head <$> xs
    case x of
      Nothing -> queryNotFound $ "query没有" <> key <> "字段！"
      Just x' -> mapLeft QueryParseFail $ parseParam x'

tryQuery :: forall e m a. Parsable a => Monad m => String -> ActionT e m (Maybe a)
tryQuery key = hush <$> query key

query' :: forall e m a. Parsable a => Monad m => a -> String -> ActionT e m a
query' def key = fromMaybeAction (tryQuery key) def
