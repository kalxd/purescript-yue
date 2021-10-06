module Yue.Server.Action ( mapActionT
                         , shadowActionError
                         , askState
                         , asksState
                         , getState
                         , getsState
                         , putState
                         , modifyState
                         ) where

import Prelude

import Control.Monad.Except.Trans (mapExceptT)
import Control.Monad.Reader.Trans (class MonadReader, ask, asks, mapReaderT)
import Control.Monad.State.Trans (class MonadState, get, gets, mapStateT, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Yue.Internal.Type.Action (ActionST(..), ActionT)
import Yue.Internal.Util (mapLeft)

mapActionT :: forall e m1 m2 a. (forall b. m1 b -> m2 b) -> ActionT e m1 a -> ActionT e m2 a
mapActionT f = mapExceptT g
  where g = mapReaderT h
        h = mapStateT f

shadowActionError :: forall e1 e2 m a. Monad m => (e1 -> e2) -> ActionT e1 m (Either e2 a) -> ActionT e2 m a
shadowActionError f = mapExceptT \ma -> do
  a <- ma
  pure $ case a of
    Right a' -> mapLeft ActionError a'
    Left e -> Left $ f <$> e

askState :: forall e m a. MonadReader a m => ActionT e m a
askState = lift $ lift ask

asksState :: forall e m a a'. MonadReader a m => (a -> a') -> ActionT e m a'
asksState = lift <<< lift <<< asks

getState :: forall e m s. MonadState s m => ActionT e m s
getState = lift $ lift $ lift get

getsState :: forall e m s s'. MonadState s m => (s -> s') -> ActionT e m s'
getsState = lift <<< lift <<< lift <<< gets

putState :: forall e m s. MonadState s m => s -> ActionT e m Unit
putState = lift <<< lift <<< lift <<< put

modifyState :: forall e m s. MonadState s m => (s -> s) -> ActionT e m s
modifyState = lift <<< lift <<< lift <<< modify
