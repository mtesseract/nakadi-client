{-|
Module      : Network.Nakadi.Internal.Types
Description : Nakadi Client Types (Internal)
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Exports all types for internal usage.
-}

{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE UndecidableSuperClasses    #-}

module Network.Nakadi.Internal.Types
  ( module Network.Nakadi.Internal.Types.Config
  , module Network.Nakadi.Internal.Types.Exceptions
  , module Network.Nakadi.Internal.Types.Logger
  , module Network.Nakadi.Internal.Types.Problem
  , module Network.Nakadi.Internal.Types.Service
  , module Network.Nakadi.Internal.Types.Subscription
  , module Network.Nakadi.Internal.Types.Util
  , MonadNakadi(..)
  , MonadNakadiHttpStream(..)
  , MonadNakadiHttp(..)
  , NakadiT(..)
  , runNakadiT
  ) where

import           Control.Lens                               ((<&>))
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.State.Class
import qualified Control.Monad.State.Lazy                   as State.Lazy
import qualified Control.Monad.State.Strict                 as State.Strict
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader                 (ReaderT (..))
import           Control.Monad.Trans.Resource.Internal      (transResourceT)
import qualified Data.ByteString.Lazy                       as LB
import           Data.Conduit                               (transPipe)
import           Data.Conduit.Internal                      (ConduitM (..),
                                                             Pipe (..))
import           GHC.Exts                                   (Constraint)
import           Network.HTTP.Client                        (Manager, httpLbs,
                                                             responseClose,
                                                             responseOpen)
import           Network.HTTP.Client.Conduit                (bodyReaderSource)
import           Network.HTTP.Client.TLS                    (getGlobalManager)
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Retry
import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Exceptions
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Problem
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription
import           Network.Nakadi.Internal.Types.Util
import           UnliftIO                                   hiding (mask,
                                                             uninterruptibleMask)

-- * Define Typeclasses

-- | The `MonadNakadiHttp` typeclass provides a method
-- (`nakadiHttpLbs`) for executing single HTTP requests. The first
-- parameter (`b`) denotes the configuration's `base monad`. This is
-- the monad in which callbacks registered in the configuration are
-- run. The second parameter (`m`) denotes the monad in which the
-- method `nakadiHttpLbs` runs. Using an associated type family
-- (`NakadiHttpConstraint`) of kind `Constraint`, this typeclass can
-- announce a typeclass constraint which it requires to run.

class (Monad b, Monad m) => MonadNakadiHttp b m | m -> b where
  -- | Retrying via Control.Retry requires the `MonadMask` constraint.
  -- But pure monad transformer stacks are tricky with MonadMask, thus
  -- we let the `MonadNakadiHttp` instances specify their own
  -- constraint.
  type NakadiHttpConstraint m :: Constraint
  nakadiHttpLbs :: Config b
                -> Request
                -> Maybe Manager
                -> m (Response LB.ByteString)

class (Monad b, Monad m) => MonadNakadiHttpStream b m | m -> b where
  -- | Retrying via Control.Retry requires the `MonadMask` constraint.
  -- But pure monad transformer stacks are tricky with MonadMask, thus
  -- we let the `MonadNakadiHttp` instances specify their own
  -- constraint.
  type NakadiHttpStreamConstraint m :: Constraint
  nakadiHttpResponseOpen :: Request
                         -> Maybe Manager
                         -> m (Response (ConduitM () ByteString m ()))
  nakadiHttpResponseClose :: Response a
                          -> m ()

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
class (MonadNakadiHttp b m, MonadThrow b, MonadThrow m, MonadCatch m)
   => MonadNakadi b m | m -> b where
  -- {-# MINIMAL (nakadiAsk | nakadiReader), nakadiLocal #-}
  nakadiAsk :: m (Config b)
  nakadiAsk = nakadiReader identity
  nakadiLocal :: (Config b -> Config b) -> m a -> m a
  nakadiReader :: (Config b -> a) -> m a
  nakadiReader f = nakadiAsk >>= (return . f)
  nakadiLift :: b a -> m a

-- * Define Types

-- | The `NakadiT` type is just a specialized `ReaderT` monad.
newtype NakadiT b m a = NakadiT
  { _runNakadiT :: Config b -> m a
  }

-- * Provide Typeclass Implemenations

-- ** Implement general typeclass instances for `NakadiT`.

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

-- | `Functor` for `NakadiT`.
instance Functor m => Functor (NakadiT b m) where
  fmap f (NakadiT n) = NakadiT (\c -> fmap f (n c))

-- | `Monad` for `NakadiT`.
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

-- | `MonadTrans` for `NakadiT`.
instance MonadTrans (NakadiT b) where
    lift a = NakadiT (const a)
    {-# INLINE lift #-}

-- | `MonadThrow` for `NakadiT`.
instance (Monad b, MonadThrow m) => MonadThrow (NakadiT b m) where
  throwM e = lift $ Control.Monad.Catch.throwM e

-- | `MonadCatch` for `NakadiT`.
instance (Monad b, MonadCatch m) => MonadCatch (NakadiT b m) where
  catch (NakadiT b) h =
    NakadiT $ \ c -> b c `Control.Monad.Catch.catch` \e -> _runNakadiT (h e) c

-- | `MonadMask` for `NakadiT`.
instance (Monad b, MonadMask m) => MonadMask (NakadiT b m) where
  mask a = NakadiT $ \e -> mask $ \u -> _runNakadiT (a $ q u) e
    where q :: (m a -> m a) -> NakadiT e m a -> NakadiT e m a
          q u (NakadiT b) = NakadiT (u . b)
  uninterruptibleMask a =
    NakadiT $ \e -> uninterruptibleMask $ \u -> _runNakadiT (a $ q u) e
      where q :: (m a -> m a) -> NakadiT e m a -> NakadiT e m a
            q u (NakadiT b) = NakadiT (u . b)

-- | `MonadIO` for `NakadiT`.
instance (Monad b, MonadIO m) => MonadIO (NakadiT b m) where
  liftIO = lift . liftIO

-- | `MonadBase` for `NakadiT`.
instance (Monad m, MonadBase b' m) => MonadBase b' (NakadiT b m) where
  liftBase = liftBaseDefault

-- | `MonadReader` for `NakadiT`.
instance (Monad b, MonadReader r m) => MonadReader r (NakadiT b m) where
  ask = lift ask
  local = mapNakadiT . local

-- | `MonadState` for `NakadiT`.
instance (Monad b, MonadState s m) => MonadState s (NakadiT b m) where
  get = lift get
  put = lift . put

-- | `MonadState` for `NakadiT`.
instance (Monad b, MonadUnliftIO m) => MonadUnliftIO (NakadiT b m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO =
    NakadiT $ \r ->
    withUnliftIO $ \u ->
    return (UnliftIO (unliftIO u . runNakadiT r))

-- | `MonadTransControl` for `NakadiT`.
instance MonadTransControl (NakadiT b) where
  type StT (NakadiT b) a = a
  liftWith f = NakadiT $ \r -> f $ \t -> _runNakadiT t r
  restoreT = NakadiT . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

-- | Inherit `MonadBaseControl` for `NakadiT`.
instance MonadBaseControl b' m => MonadBaseControl b' (NakadiT b m) where
  type StM (NakadiT b m) a = ComposeSt (NakadiT b) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

-- ** Implementations for `MonadNakadiHttp`.

getHttpManager :: MonadIO m => Maybe Manager -> m Manager
getHttpManager Nothing        = liftIO getGlobalManager
getHttpManager (Just manager) = pure manager

-- | `IO`-based implementation of `MonadNakadiHttp` for `NakadiT`
-- using retries.
instance ( MonadIO b
         , MonadUnliftIO b
         , MonadMask b
         , MonadBase IO b
         ) => MonadNakadiHttp b (NakadiT b b) where
  type NakadiHttpConstraint (NakadiT b b) = MonadMask (NakadiT b b)
  nakadiHttpLbs config req maybeMngr = lift $ do
    mngr <- getHttpManager maybeMngr
    retryAction config req (\r -> liftIO $ httpLbs r mngr)

-- | Inherit `MonadNakadiHttp` implementation to `ReaderT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (ReaderT r m) where
  type NakadiHttpConstraint (ReaderT r m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `StateT` (strict version).
instance MonadNakadiHttp b m => MonadNakadiHttp b (State.Strict.StateT s m) where
  type NakadiHttpConstraint (State.Strict.StateT s m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `StateT` (lazy version).
instance MonadNakadiHttp b m => MonadNakadiHttp b (State.Lazy.StateT s m) where
  type NakadiHttpConstraint (State.Lazy.StateT s m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `ResourceT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (ResourceT m) where
  type NakadiHttpConstraint (ResourceT m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `LoggingT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (LoggingT m) where
  type NakadiHttpConstraint (LoggingT m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `NoLoggingT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (NoLoggingT m) where
  type NakadiHttpConstraint (NoLoggingT m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `ConduitM`.
instance (MonadNakadiHttp b m) => MonadNakadiHttp b (ConduitM i o m) where
  type NakadiHttpConstraint (ConduitM i o m) = MonadMask m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- ** Implementations for `MonadNakadiHttpStream`.

-- | `IO`-based implementation of `MonadNakadiHttpStream` for
-- `NakadiT` using retries.
instance (m ~ NakadiT b b, MonadIO m, MonadMask m, MonadBase IO b)
      => MonadNakadiHttpStream b (NakadiT b b) where
  type NakadiHttpStreamConstraint (NakadiT b b) = MonadMask (NakadiT b b)
  nakadiHttpResponseOpen req maybeMngr = do
    mngr <- getHttpManager maybeMngr
    liftIO $ responseOpen req mngr <&> fmap (transPipe liftIO . bodyReaderSource)
  nakadiHttpResponseClose = liftIO . responseClose

-- | Inherit `MonadNakadiHttpStream` implementation to `ReaderT`.
instance (MonadNakadiHttpStream b m)
      => MonadNakadiHttpStream b (ReaderT r m) where
  type NakadiHttpStreamConstraint (ReaderT r m) = MonadMask m
  nakadiHttpResponseOpen req mngr =
    lift (nakadiHttpResponseOpen req mngr)
    <&> fmap (transPipe lift)
  nakadiHttpResponseClose = lift . nakadiHttpResponseClose

-- | Inherit `MonadNakadiHttpStream` implementation to `StateT`
-- (strict).
instance (MonadNakadiHttpStream b m)
      => MonadNakadiHttpStream b (State.Strict.StateT s m) where
  type NakadiHttpStreamConstraint (State.Strict.StateT s m) = MonadMask m
  nakadiHttpResponseOpen req mngr =
    lift (nakadiHttpResponseOpen req mngr)
    <&> fmap (transPipe lift)
  nakadiHttpResponseClose = lift . nakadiHttpResponseClose

-- | Inherit `MonadNakadiHttpStream` implementation to `StateT`
-- (lazy).
instance (MonadNakadiHttpStream b m)
      => MonadNakadiHttpStream b (State.Lazy.StateT s m) where
  type NakadiHttpStreamConstraint (State.Lazy.StateT s m) = MonadMask m
  nakadiHttpResponseOpen req mngr =
    lift (nakadiHttpResponseOpen req mngr)
    <&> fmap (transPipe lift)
  nakadiHttpResponseClose = lift . nakadiHttpResponseClose

-- | Inherit `MonadNakadiHttpStream` implementation to `ResourceT`.
instance (MonadNakadiHttpStream b m)
      => MonadNakadiHttpStream b (ResourceT m) where
  type NakadiHttpStreamConstraint (ResourceT m) = MonadMask m
  nakadiHttpResponseOpen req mngr =
    lift (nakadiHttpResponseOpen req mngr)
    <&> fmap (transPipe lift)
  nakadiHttpResponseClose = lift . nakadiHttpResponseClose

-- | Inherit `MonadNakadiHttp` implementation to `LoggingT`.
instance (MonadNakadiHttpStream b m) => MonadNakadiHttpStream b (LoggingT m) where
  type NakadiHttpStreamConstraint (LoggingT m) = MonadMask m
  nakadiHttpResponseOpen req mngr =
    lift (nakadiHttpResponseOpen req mngr)
    <&> fmap (transPipe lift)
  nakadiHttpResponseClose = lift . nakadiHttpResponseClose

-- | Inherit `MonadNakadiHttp` implementation to `NoLoggingT`.
instance (MonadNakadiHttpStream b m) => MonadNakadiHttpStream b (NoLoggingT m) where
  type NakadiHttpStreamConstraint (NoLoggingT m) = MonadMask m
  nakadiHttpResponseOpen req mngr =
    lift (nakadiHttpResponseOpen req mngr)
    <&> fmap (transPipe lift)
  nakadiHttpResponseClose = lift . nakadiHttpResponseClose

-- ** Implementations for `MonadNakadi`.

-- | Implementation of `MonadNakadi` for `NakadiT`.
instance ( MonadNakadiHttp b (NakadiT b b)
         , MonadBase IO b
         , MonadIO b
         , MonadCatch b )
      => MonadNakadi b (NakadiT b b) where
  nakadiAsk = NakadiT return
  nakadiLocal f (NakadiT m) = NakadiT (\ c -> m (f c))
  nakadiLift = NakadiT . const

-- | Inherit `MonadNakadi` implementation to `ReaderT`.
instance (MonadNakadi b m) => MonadNakadi b (ReaderT r m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal f (ReaderT m) = ReaderT (\ r -> nakadiLocal f (m r))
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `StateT` (strict).
instance (MonadNakadi b m) => MonadNakadi b (State.Strict.StateT s m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal f (State.Strict.StateT g) =
    State.Strict.StateT $ \ s -> nakadiLocal f (g s)
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `StateT` (lazy).
instance (MonadNakadi b m) => MonadNakadi b (State.Lazy.StateT s m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal f (State.Lazy.StateT g) =
    State.Lazy.StateT $ \ s -> nakadiLocal f (g s)
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `LoggingT`.
instance (MonadNakadi b m) => MonadNakadi b (LoggingT m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal f = LoggingT . (nakadiLocal f .) . runLoggingT
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `NoLoggingT`.
instance (MonadNakadi b m) => MonadNakadi b (NoLoggingT m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal f = NoLoggingT . (nakadiLocal f) . runNoLoggingT
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `ResourceT`.
instance (MonadNakadi b m) => MonadNakadi b (ResourceT m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal f m = transResourceT (nakadiLocal f) m
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `ConduitM`.
instance MonadNakadi b m => MonadNakadi b (ConduitM i o m) where
  nakadiAsk = lift nakadiAsk
  {-# INLINE nakadiAsk #-}
  nakadiLocal f (ConduitM c0) = ConduitM $ \rest ->
    let go (HaveOutput p c o) = HaveOutput (go p) c o
        go (NeedInput p c)    = NeedInput (\i -> go (p i)) (\u -> go (c u))
        go (Done x)           = rest x
        go (PipeM mp)         = PipeM (liftM go $ nakadiLocal f mp)
        go (Leftover p i)     = Leftover (go p) i
    in go (c0 Done)
  nakadiLift = lift . nakadiLift

-- * Convenient Functions

runNakadiT :: Config b -> NakadiT b m a -> m a
runNakadiT config a = _runNakadiT a config

mapNakadiT :: (m a -> m a) -> NakadiT b m a -> NakadiT b m a
mapNakadiT f n = NakadiT $ \ c -> f (_runNakadiT n c)
