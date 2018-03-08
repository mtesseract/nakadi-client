{-|
Module      : Network.Nakadi.Internal.Types
Description : Nakadi Client Types (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Exports all types for internal usage.
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Nakadi.Internal.Types
  ( module Network.Nakadi.Internal.Types.Config
  , module Network.Nakadi.Internal.Types.Exceptions
  , module Network.Nakadi.Internal.Types.Logger
  , module Network.Nakadi.Internal.Types.Problem
  , module Network.Nakadi.Internal.Types.Service
  , module Network.Nakadi.Internal.Types.Util
  , module Network.Nakadi.Internal.Types.Base
  , module Network.Nakadi.Internal.Types.Subscriptions
  , HasNakadiConfig(..)
  , MonadNakadi(..)
  , MonadNakadiIO
  , NakadiT(..)
  , runNakadiT
  ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.State.Class
import qualified Control.Monad.State.Lazy                    as State.Lazy
import qualified Control.Monad.State.Strict                  as State.Strict
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader                  (ReaderT (..))
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Writer.Lazy                   as Writer.Lazy
import qualified Control.Monad.Writer.Strict                 as Writer.Strict
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Types.Base
import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Exceptions
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Problem
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscriptions
import           Network.Nakadi.Internal.Types.Util

-- * Define Typeclasses

class HasNakadiConfig b r | r -> b where
  nakadiConfig :: r -> Config b

-- | The `MonadNakadi` typeclass is implemented by monads in which
-- Nakadi can be called. The first parameter (`b`) denotes the `base
-- monad`. This is the monad in which the core actions are run. This
-- includes executing (non-streaming) HTTP requests and running
-- user-provided callbacks. The typeclass provides methods for
-- * retrieving the Nakadi configuration
-- * locally changing the Nakadi configuration
-- * extracting specific Nakadi configuration values
-- * lifting actions from the
-- The `MonadNakadi` typeclass is modelled closely after `MonadReader`.
class (MonadNakadiBase b m, MonadThrow b, MonadMask b, MonadThrow m, MonadCatch m)
   => MonadNakadi b m | m -> b where
  nakadiAsk :: m (Config b)
  default nakadiAsk :: (MonadNakadi b n, MonadTrans t, m ~ t n) => m (Config b)
  nakadiAsk = lift nakadiAsk

-- * Define Types

type MonadNakadiIO = MonadNakadi IO

-- | The `NakadiT` type is just a specialized `ReaderT` monad.
newtype NakadiT b m a = NakadiT { _runNakadiT :: Config b -> m a }

-- * Provide Typeclass Implementaions

-- ** Implement typeclass instances for `NakadiT`.

-- | `Functor` for `NakadiT`.
instance Functor m => Functor (NakadiT b m) where
  fmap f (NakadiT n) = NakadiT (\c -> fmap f (n c))

-- | `Applicative` for `NakadiT`.
instance (Applicative m) => Applicative (NakadiT b m) where
  pure a  = NakadiT $ \_conf -> pure a
  {-# INLINE pure #-}
  f <*> v = NakadiT $ \ c -> _runNakadiT f c <*> _runNakadiT v c
  {-# INLINE (<*>) #-}
  u *> v = NakadiT $ \ c -> _runNakadiT u c *> _runNakadiT v c
  {-# INLINE (*>) #-}
  u <* v = NakadiT $ \ c -> _runNakadiT u c <* _runNakadiT v c
  {-# INLINE (<*) #-}

-- | 'Monad'
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

-- | 'MonadTrans'
instance MonadTrans (NakadiT b) where
    lift a = NakadiT (const a)
    {-# INLINE lift #-}

-- | 'MonadThrow'
instance (Monad b, MonadThrow m) => MonadThrow (NakadiT b m) where
  throwM e = lift $ Control.Monad.Catch.throwM e

-- | 'MonadCatch'
instance (Monad b, MonadCatch m) => MonadCatch (NakadiT b m) where
  catch (NakadiT b) h =
    NakadiT $ \ c -> b c `Control.Monad.Catch.catch` \e -> _runNakadiT (h e) c

-- | 'MonadMask'
instance (Monad b, MonadMask m) => MonadMask (NakadiT b m) where
  mask a = NakadiT $ \e -> mask $ \u -> _runNakadiT (a $ q u) e
    where q :: (m a -> m a) -> NakadiT e m a -> NakadiT e m a
          q u (NakadiT b) = NakadiT (u . b)
  uninterruptibleMask a =
    NakadiT $ \e -> uninterruptibleMask $ \u -> _runNakadiT (a $ q u) e
      where q :: (m a -> m a) -> NakadiT e m a -> NakadiT e m a
            q u (NakadiT b) = NakadiT (u . b)

-- | 'MonadIO'
instance (Monad b, MonadIO m) => MonadIO (NakadiT b m) where
  liftIO = lift . liftIO

-- | 'MonadBase'
instance (Monad m, MonadBase b' m) => MonadBase b' (NakadiT b m) where
  liftBase = liftBaseDefault

-- | 'MonadReader'
instance (Monad b, MonadReader r m) => MonadReader r (NakadiT b m) where
  ask = lift ask
  local = mapNakadiT . local

-- | 'MonadLogger'
instance MonadLogger m => MonadLogger (NakadiT b m)

-- | 'MonadLoggerIO'
instance (Monad b, MonadLoggerIO m) => MonadLoggerIO (NakadiT b m)

-- | 'MonadState'
instance (Monad b, MonadState s m) => MonadState s (NakadiT b m) where
  get = lift get
  put = lift . put

instance (Monad b, MonadResource m) => MonadResource (NakadiT b m) where
  liftResourceT = lift . liftResourceT

-- | 'MonadUnliftIO'
instance (Monad b, MonadUnliftIO m) => MonadUnliftIO (NakadiT b m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO =
    NakadiT $ \r ->
    withUnliftIO $ \u ->
    return (UnliftIO (unliftIO u . runNakadiT r))

-- | 'MonadTransControl'
instance MonadTransControl (NakadiT b) where
  type StT (NakadiT b) a = a
  liftWith f = NakadiT $ \r -> f $ \t -> _runNakadiT t r
  restoreT = NakadiT . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

-- | 'MonadBaseControl'
instance MonadBaseControl b' m => MonadBaseControl b' (NakadiT b m) where
  type StM (NakadiT b m) a = ComposeSt (NakadiT b) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

-- | 'MonadNakadiBase'
instance {-# OVERLAPPABLE #-} MonadNakadiBase b m => MonadNakadiBase b (NakadiT b m)

-- ** Implementations 'MonadNakadi' typeclass for transformers.

-- | 'ReaderT'
instance ( MonadMask b
         , MonadCatch m
         , MonadNakadiBase b (ReaderT r m)
         , HasNakadiConfig b r )
      => MonadNakadi b (ReaderT r m) where
  nakadiAsk = asks nakadiConfig

-- | 'NakadiT'
instance ( MonadCatch m
         , MonadMask b
         , MonadNakadiBase b (NakadiT b m) )
      => MonadNakadi b (NakadiT b m) where
  nakadiAsk = NakadiT return

-- | 'WriterT' (lazy)
instance (MonadNakadi b m, Monoid w) => MonadNakadi b (Writer.Lazy.WriterT w m)

-- | 'WriterT' (strict)
instance (MonadNakadi b m, Monoid w) => MonadNakadi b (Writer.Strict.WriterT w m)

-- | 'StateT' (strict)
instance (MonadNakadi b m) => MonadNakadi b (State.Strict.StateT s m)

-- | 'StateT' (lazy)
instance (MonadNakadi b m) => MonadNakadi b (State.Lazy.StateT s m)

-- | 'LoggingT'
instance (MonadNakadi b m) => MonadNakadi b (LoggingT m)

-- | 'NoLoggingT'
instance (MonadNakadi b m) => MonadNakadi b (NoLoggingT m)

-- | 'ResourceT'.
instance (MonadNakadi b m) => MonadNakadi b (ResourceT m)

-- * Convenience Functions

runNakadiT :: Config b -> NakadiT b m a -> m a
runNakadiT = flip _runNakadiT

mapNakadiT :: (m a -> m a) -> NakadiT b m a -> NakadiT b m a
mapNakadiT f n = NakadiT $ \ c -> f (_runNakadiT n c)
