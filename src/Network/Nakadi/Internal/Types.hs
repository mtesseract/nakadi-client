{-# LANGUAGE UndecidableSuperClasses    #-}
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

module Network.Nakadi.Internal.Types
  ( module Network.Nakadi.Internal.Types.Config
  , module Network.Nakadi.Internal.Types.Exceptions
  , module Network.Nakadi.Internal.Types.Logger
  , module Network.Nakadi.Internal.Types.Problem
  , module Network.Nakadi.Internal.Types.Service
  , module Network.Nakadi.Internal.Types.Subscription
  , module Network.Nakadi.Internal.Types.Util
  , MonadNakadi(..)
  , MonadNakadiHttp(..)
  , MonadNakadiHttpStream(..)
  , NakadiT(..)
  , runNakadiT
  , liftSub
  )
where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader                 (ReaderT (..))
import           Control.Monad.Trans.Resource               (MonadResource (..),
                                                             allocate,
                                                             transResourceT)
import qualified Data.ByteString.Lazy                       as LB
import           Data.Conduit                               (transPipe)
import           Data.Conduit.Internal                      (ConduitM (..),
                                                             Pipe (..))
import           GHC.Exts                                   (Constraint)
import           Network.HTTP.Client                        (Manager, httpLbs,
                                                             responseClose,
                                                             responseOpen)
import           Network.HTTP.Client.Conduit                (bodyReaderSource)
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Retry
import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Exceptions
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Problem
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription
import           Network.Nakadi.Internal.Types.Util
import           UnliftIO

-- * Define Typeclasses

-- | The `MonadNakadiHttp` typeclass provides a method
-- (`nakadiHttpLbs`) for executing single HTTP requests. The first
-- parameter (`b`) denotes the configuration's `base monad`. This is
-- the monad in which callbacks registered in the configuration are
-- run. The second parameter (`m`) denotes the monad in which the
-- method `nakadiHttpLbs` runs. Using an associated type family
-- (`NakadiHttpConstraint`) of kind `Constraint`, this typeclass can
-- announce a typeclass constraint which it requires to run.

class (Monad b, Monad m) => MonadNakadiHttp b m where
  type NakadiHttpConstraint m :: Constraint
  nakadiHttpLbs :: NakadiHttpConstraint m
                => Config b -> Request -> Manager -> m (Response LB.ByteString)

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
class (MonadNakadiHttp b m, MonadThrow m, MonadCatch m, NakadiHttpConstraint m)
   => MonadNakadi b m | m -> b where
  -- {-# MINIMAL (nakadiAsk | nakadiReader), local #-}
  nakadiAsk :: m (Config b)
  nakadiAsk = nakadiReader identity
  nakadiLocal :: (Config b -> Config b) -> m a -> m a
  nakadiReader :: (Config b -> a) -> m a
  nakadiReader f = nakadiAsk >>= (return . f)
  nakadiLift :: b a -> m a -- nakadiLift = lift

-- | The `MonadNakadiHttpStream` typeclass is to be implemented on top
-- of `MonadNakadi`. It provides additional functionality for
-- streaming HTTP requests. Using the associated type family
-- `NakadiHttpStreamConstraint` implementations of this typeclass can
-- announce additional constraints required by the provided
-- `nakadiHttpStream` method.
class (MonadNakadi b m) => MonadNakadiHttpStream b m where
  type NakadiHttpStreamConstraint m :: Constraint
  nakadiHttpStream :: NakadiHttpStreamConstraint m
                   => Config b
                   -> Request
                   -> Manager
                   -> m (Response (ConduitM () ByteString m ()))

-- * Define Types

-- | The `NakadiT` type is just a specialized `ReaderT` monad.
newtype NakadiT b m a = NakadiT
  { _runNakadiT :: Config b -> m a
  }


-- * Provide Typeclass Implemenations

-- ** Implement general typeclass instances for `NakadiT`.

instance (Applicative m) => Applicative (NakadiT b m) where
  pure a  = NakadiT $ \_conf -> pure a
  {-# INLINE pure #-}
  f <*> v = NakadiT $ \ c -> _runNakadiT f c <*> _runNakadiT v c
  {-# INLINE (<*>) #-}
  u *> v = NakadiT $ \ c -> _runNakadiT u c *> _runNakadiT v c
  {-# INLINE (*>) #-}
  u <* v = NakadiT $ \ c -> _runNakadiT u c <* _runNakadiT v c
  {-# INLINE (<*) #-}

instance Functor m => Functor (NakadiT b m) where
  fmap f (NakadiT n) = NakadiT (\c -> fmap f (n c))

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

instance MonadTrans (NakadiT b) where
    lift a = NakadiT (const a)
    {-# INLINE lift #-}

instance (Monad b, MonadThrow m) => MonadThrow (NakadiT b m) where
  throwM e = lift $ Control.Monad.Catch.throwM e

instance (Monad b, MonadCatch m) => MonadCatch (NakadiT b m) where
  catch (NakadiT b) h =
    NakadiT $ \ c -> b c `Control.Monad.Catch.catch` \e -> _runNakadiT (h e) c

instance (Monad b, MonadIO m) => MonadIO (NakadiT b m) where
  liftIO = lift . liftIO

instance (Monad m, MonadBase b' m) => MonadBase b' (NakadiT b m) where
  liftBase = liftBaseDefault

instance (Monad b, MonadReader r m) => MonadReader r (NakadiT b m) where
  ask = lift ask
  local = mapNakadiT . local

instance (Monad b, MonadState s m) => MonadState s (NakadiT b m) where
  get = lift get
  put = lift . put

instance (Monad b, MonadUnliftIO m) => MonadUnliftIO (NakadiT b m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO =
    NakadiT $ \r ->
    withUnliftIO $ \u ->
    return (UnliftIO (unliftIO u . runNakadiT r))

instance MonadTransControl (NakadiT b) where
  type StT (NakadiT b) a = a
  liftWith f = NakadiT $ \r -> f $ \t -> _runNakadiT t r
  restoreT = NakadiT . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (NakadiT b m) where
  type StM (NakadiT b m) a = ComposeSt (NakadiT b) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

-- ** Implementations for `MonadNakadiHttp`.

-- | `IO`-based implementation of `MonadNakadiHttp` for `NakadiT`
-- using retries and `httpLbs`.
instance (MonadIO b) => MonadNakadiHttp b (NakadiT b b) where
  type NakadiHttpConstraint (NakadiT b b) = MonadMask b
  nakadiHttpLbs config req mngr = lift $
    retryAction config req (\r -> liftIO $ httpLbs r mngr)

-- | Inherit `MonadNakadiHttp` implementation to `ReaderT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (ReaderT r m) where
  type NakadiHttpConstraint (ReaderT r m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `ResourceT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (ResourceT m) where
  type NakadiHttpConstraint (ResourceT m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `ConduitM`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (ConduitM i o m) where
  type NakadiHttpConstraint (ConduitM i o m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- | Inherit `MonadNakadiHttp` implementation to `LoggingT`.
instance MonadNakadiHttp b m => MonadNakadiHttp b (LoggingT m) where
  type NakadiHttpConstraint (LoggingT m) = NakadiHttpConstraint m
  nakadiHttpLbs conf req mngr = lift $ nakadiHttpLbs conf req mngr

-- ** Implementations for `MonadNakadi`.

-- | Implementation of `MonadNakadi` for `NakadiT`.
instance (MonadNakadiHttp b (NakadiT b b), MonadBase IO b, MonadIO b, MonadMask b)
      => MonadNakadi b (NakadiT b b) where
  nakadiAsk = NakadiT return
  nakadiReader f = NakadiT (return . f)
  nakadiLocal f (NakadiT m) = NakadiT (\ c -> m (f c))
  nakadiLift = NakadiT . const

-- | Inherit `MonadNakadi` implementation to `ReaderT`.
instance (MonadNakadi b m) => MonadNakadi b (ReaderT r m) where
  nakadiAsk = lift nakadiAsk
  nakadiReader = lift . nakadiReader
  nakadiLocal f (ReaderT m) = ReaderT (\ r -> nakadiLocal f (m r))
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `LoggingT`.
instance (MonadNakadi b m) => MonadNakadi b (LoggingT m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal = mapLoggingT . nakadiLocal
  nakadiLift = lift . nakadiLift

-- | Inherit `MonadNakadi` implementation to `ResourceT`.
instance (MonadNakadi b m) => MonadNakadi b (ResourceT m) where
  nakadiAsk = lift nakadiAsk
  nakadiReader = lift . nakadiReader
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

-- * ....

instance (MonadNakadi b m) => MonadNakadiHttpStream b (ResourceT m) where
  type NakadiHttpStreamConstraint (ResourceT b) = MonadResource (ResourceT b)
  nakadiHttpStream _config req mngr = do
    (_, rsp) <- allocate (responseOpen req mngr) responseClose
    pure $ bodyReaderSource <$> rsp

instance MonadNakadiHttpStream b m => MonadNakadiHttpStream b (ReaderT r m) where
  type NakadiHttpStreamConstraint (ReaderT r m) = NakadiHttpStreamConstraint m
  nakadiHttpStream config req mngr = lift $ do
    rsp <- nakadiHttpStream config req mngr
    pure $ transPipe lift <$> rsp

instance MonadNakadiHttpStream b m => MonadNakadiHttpStream b (LoggingT m) where
  type NakadiHttpStreamConstraint (LoggingT m) = NakadiHttpStreamConstraint m
  nakadiHttpStream config req mngr = lift $ do
    rsp <- nakadiHttpStream config req mngr
    pure $ transPipe lift <$> rsp

instance MonadNakadiHttpStream b m => MonadNakadiHttpStream b (ConduitM i o m) where
  type NakadiHttpStreamConstraint (ConduitM i o m) = NakadiHttpStreamConstraint m
  nakadiHttpStream config req mngr = lift $ do
    rsp <- nakadiHttpStream config req mngr
    pure $ transPipe lift <$> rsp

--------------------------------------------------------------------------------

-- * Convenient Functions

runNakadiT :: Config b -> NakadiT b m a -> m a
runNakadiT config a = _runNakadiT a config

mapLoggingT :: (m a -> n b) -> LoggingT m a -> LoggingT n b
mapLoggingT f = LoggingT . (f .) . runLoggingT

mapNakadiT :: (m a -> m a) -> NakadiT b m a -> NakadiT b m a
mapNakadiT f n = NakadiT $ \ c -> f (_runNakadiT n c)

liftSub :: MonadNakadi b m => b a -> m a
liftSub = nakadiLift
