{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances #-}

import Control.Monad.Trans (MonadTrans, MonadIO, lift, liftIO)
import Control.Monad (liftM)
import Control.Monad.State (MonadState, get, put)

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f =
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=) = bindMT
    fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put k = lift (put k)
