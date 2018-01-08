{-|
Module      : Network.Nakadi.Internal.Types
Description : Nakadi Client Types (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Exports all types for internal usage.
-}

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Network.Nakadi.Internal.Types
  ( module Network.Nakadi.Internal.Types.Config
  , module Network.Nakadi.Internal.Types.Exceptions
  , module Network.Nakadi.Internal.Types.Logger
  , module Network.Nakadi.Internal.Types.Problem
  , module Network.Nakadi.Internal.Types.Service
  , module Network.Nakadi.Internal.Types.Subscription
  , module Network.Nakadi.Internal.Types.Util
  , MonadNakadi
  , MonadNakadiEnv(..)
  , MonadSub(..)
  , NakadiT
  , runNakadiT
  ) where

import           Control.Monad.Catch
import           Control.Monad.Trans.Reader                 (ReaderT)
import qualified Control.Monad.Trans.Reader                 as Reader
import           Control.Monad.Trans.Resource
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Exceptions
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Problem
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription
import           Network.Nakadi.Internal.Types.Util

import           Control.Monad.Base
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control

-- | type constraint synonym for encapsulating the monad constraints
-- required by most funtions in this package.
type MonadNakadi b m = (MonadIO m, MonadIO b, MonadCatch m, MonadSub b m, MonadMask b)

class MonadNakadi b m => MonadNakadiEnv b m | m -> b where
  -- {-# MINIMAL (nakadiAsk | nakadiReader) #-}
  nakadiAsk :: m (Config b)
  nakadiAsk = nakadiReader identity
  nakadiLocal :: (Config b -> Config b) -> m a -> m a
  nakadiReader :: (Config b -> a) -> m a
  nakadiReader f = nakadiAsk >>= (return . f)

instance (MonadIO m, MonadIO b, MonadCatch m, MonadMask b, MonadSub b m)
      => MonadNakadiEnv b (ReaderT (Config b) m) where
  nakadiAsk = Reader.ask
  nakadiLocal = Reader.local
  nakadiReader = Reader.reader

class (Monad b, Monad m) => MonadSub b m where
  liftSub :: Monad m => b a -> m a

instance Monad b => MonadSub b b where
  liftSub = identity

instance (Monad m, Monad b, Monad (t m), MonadSub b m, MonadTrans t) => MonadSub b (t m) where
  liftSub = lift . liftSub

runNakadiT :: Config b -> NakadiT b m a -> m a
runNakadiT = flip _runNakadiT

newtype NakadiT b m a = NakadiT { _runNakadiT :: Config b -> m a }

instance Functor m => Functor (NakadiT b m) where
  fmap f (NakadiT n) = NakadiT (\c -> fmap f (n c))

liftNakadiT :: m a -> NakadiT b m a
liftNakadiT m = NakadiT (const m)
{-# INLINE liftNakadiT #-}

instance (Applicative m) => Applicative (NakadiT b m) where
    pure    = liftNakadiT . pure
    {-# INLINE pure #-}
    f <*> v = NakadiT $ \ c -> _runNakadiT f c <*> _runNakadiT v c
    {-# INLINE (<*>) #-}
    u *> v = NakadiT $ \ c -> _runNakadiT u c *> _runNakadiT v c
    {-# INLINE (*>) #-}
    u <* v = NakadiT $ \ c -> _runNakadiT u c <* _runNakadiT v c
    {-# INLINE (<*) #-}

instance MonadTrans (NakadiT b) where
    lift   = liftNakadiT
    {-# INLINE lift #-}

instance (Monad m) => Monad (NakadiT b m) where
    return   = lift . return
    m >>= k  = NakadiT $ \ c -> do
        a <- _runNakadiT m c
        _runNakadiT (k a) c
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    fail msg = lift (fail msg)
    {-# INLINE fail #-}

instance (MonadIO m) => MonadIO (NakadiT b m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (MonadIO m, MonadIO b, MonadCatch m, MonadMask b, MonadSub b m)
      => MonadNakadiEnv b (NakadiT b m) where
  nakadiAsk = NakadiT return
  nakadiReader f = NakadiT (return . f)
  nakadiLocal f (NakadiT m) = NakadiT (\ c -> m (f c))

instance (MonadNakadiEnv b m) => MonadNakadiEnv b (ReaderT r m) where
  nakadiAsk = lift nakadiAsk
  nakadiReader = lift . nakadiReader
  nakadiLocal f (Reader.ReaderT m) = Reader.ReaderT (\ r -> nakadiLocal f (m r))

instance (MonadNakadiEnv b m) => MonadNakadiEnv b (ResourceT m) where
  nakadiAsk = lift nakadiAsk
  nakadiReader = lift . nakadiReader
  nakadiLocal f m = transResourceT (nakadiLocal f) m

instance (MonadResource m) => MonadResource (NakadiT b m) where
  liftResourceT = lift . liftResourceT

instance MonadThrow m => MonadThrow (NakadiT b m) where
  throwM e = lift $ Control.Monad.Catch.throwM e

instance MonadCatch m => MonadCatch (NakadiT b m) where
  catch (NakadiT m) h = NakadiT $ \c -> m c `Control.Monad.Catch.catch` \e -> _runNakadiT (h e) c

instance (Monad m, MonadBase IO m) => MonadBase IO (NakadiT b m) where
  liftBase = liftBaseDefault

instance MonadTransControl (NakadiT b) where
  type StT (NakadiT b) a = a
  liftWith f = NakadiT $ \c -> f $ \t -> _runNakadiT t c
  restoreT = NakadiT . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl IO m => MonadBaseControl IO (NakadiT b m) where
  type StM (NakadiT b m) a = ComposeSt (NakadiT b) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}
