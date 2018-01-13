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
-- import           Control.Monad.Trans.Control
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
import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Exceptions
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Problem
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription
import           Network.Nakadi.Internal.Types.Util
import           UnliftIO

newtype NakadiT b a = NakadiT
  { _runNakadiT :: Config b -> b a
  }

class (Monad b) => MonadNakadiHttp b where
  nakadiHttpLbs :: Request -> Manager -> b (Response LB.ByteString)

class (MonadNakadi b m) => MonadNakadiHttpStream b m where
  type NakadiHttpStreamConstraint m :: Constraint
  nakadiHttpStream :: NakadiHttpStreamConstraint m
                   => Config b
                   -> Request
                   -> Manager
                   -> m (Response (ConduitM () ByteString m ()))

-- class (MonadNakadi b m) => MonadNakadiHttpStream b m where
--   type NakadiHttpStreamConstraint m :: Constraint
--   nakadiHttpStream :: NakadiHttpStreamConstraint m
--                    => Config b
--                    -> Request
--                    -> Manager
--                    -> m (Response (ConduitM () ByteString m ()))

--------------------------------------------------------------------------------

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

instance MonadIO b => MonadNakadiHttp (NakadiT b) where
  nakadiHttpLbs req mngr = liftIO $ httpLbs req mngr

instance MonadNakadiHttp b => MonadNakadiHttp (ReaderT r b) where
  nakadiHttpLbs req mngr = lift $ nakadiHttpLbs req mngr

instance MonadNakadiHttp b => MonadNakadiHttp (ResourceT b) where
  nakadiHttpLbs req mngr = lift $ nakadiHttpLbs req mngr

instance MonadNakadiHttp b => MonadNakadiHttp (ConduitM i o b) where
  nakadiHttpLbs req mngr = lift $ nakadiHttpLbs req mngr

instance MonadNakadiHttp b => MonadNakadiHttp (LoggingT b) where
  nakadiHttpLbs req mngr = lift $ nakadiHttpLbs req mngr

runNakadiT :: Config b -> NakadiT b a -> b a
runNakadiT config a = _runNakadiT a config

instance (Applicative b) => Applicative (NakadiT b) where
    pure a  = NakadiT $ \_conf -> pure a
    {-# INLINE pure #-}
    f <*> v = NakadiT $ \ c -> _runNakadiT f c <*> _runNakadiT v c
    {-# INLINE (<*>) #-}
    u *> v = NakadiT $ \ c -> _runNakadiT u c *> _runNakadiT v c
    {-# INLINE (*>) #-}
    u <* v = NakadiT $ \ c -> _runNakadiT u c <* _runNakadiT v c
    {-# INLINE (<*) #-}

instance Functor b => Functor (NakadiT b) where
  fmap f (NakadiT n) = NakadiT (\c -> fmap f (n c))

instance (Monad b) => Monad (NakadiT b) where
    return   = lift . return
    m >>= k  = NakadiT $ \ c -> do
        a <- _runNakadiT m c
        _runNakadiT (k a) c
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    fail msg = lift (fail msg)
    {-# INLINE fail #-}

class (MonadNakadiHttp m, Monad m, MonadCatch m, MonadMask b)
   => MonadNakadi b m | m -> b where
  -- {-# MINIMAL (nakadiAsk | nakadiReader) #-}
  nakadiAsk :: m (Config b)
  nakadiAsk = nakadiReader identity
  nakadiLocal :: (Config b -> Config b) -> m a -> m a
  nakadiReader :: (Config b -> a) -> m a
  nakadiReader f = nakadiAsk >>= (return . f)
  nakadiLift :: b a -> m a -- nakadiLift = lift

instance MonadTrans NakadiT where
    lift a = NakadiT (const a)
    {-# INLINE lift #-}

instance MonadThrow b => MonadThrow (NakadiT b) where
  throwM e = lift $ Control.Monad.Catch.throwM e

instance MonadCatch b => MonadCatch (NakadiT b) where
  catch (NakadiT b) h =
    NakadiT $ \ c -> b c `Control.Monad.Catch.catch` \e -> _runNakadiT (h e) c

-- | For MonadIO.
instance MonadIO m => MonadIO (NakadiT m) where
  liftIO = lift . liftIO

-- | Implementation of MonadNakadi for NakadiT.
instance (MonadNakadiHttp b, MonadBase IO b, MonadIO b, MonadMask b)
      => MonadNakadi b (NakadiT b) where
  nakadiAsk = NakadiT return
  nakadiReader f = NakadiT (return . f)
  nakadiLocal f (NakadiT m) = NakadiT (\ c -> m (f c))
  nakadiLift = lift

liftSub :: MonadNakadi b m => b a -> m a
liftSub = nakadiLift

-- MonadNakadi Implementations for standard monad transformers.

-- | For ReaderT.
instance (MonadNakadi b m) => MonadNakadi b (ReaderT r m) where
  nakadiAsk = lift nakadiAsk
  nakadiReader = lift . nakadiReader
  nakadiLocal f (ReaderT m) = ReaderT (\ r -> nakadiLocal f (m r))
  nakadiLift = lift . nakadiLift

mapLoggingT :: (m a -> n b) -> LoggingT m a -> LoggingT n b
mapLoggingT f = LoggingT . (f .) . runLoggingT

-- | For LoggingT.
instance (MonadNakadi b m) => MonadNakadi b (LoggingT m) where
  nakadiAsk = lift nakadiAsk
  nakadiLocal = mapLoggingT . nakadiLocal
  nakadiLift = lift . nakadiLift

-- | For ResourceT.
instance (MonadNakadi b m) => MonadNakadi b (ResourceT m) where
  nakadiAsk = lift nakadiAsk
  nakadiReader = lift . nakadiReader
  nakadiLocal f m = transResourceT (nakadiLocal f) m
  nakadiLift = lift . nakadiLift

-- | For ConduitM.
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

-- MTL Type class instances for NakadiT.

mapNakadiT :: (m a -> m a) -> NakadiT m a -> NakadiT m a
mapNakadiT f n = NakadiT $ \ c -> f (_runNakadiT n c)

-- | For MonadBase IO.
instance (Monad m, MonadBase IO m) => MonadBase IO (NakadiT m) where
  liftBase = liftBaseDefault

-- | For MonadReaderT.
instance MonadReader r m => MonadReader r (NakadiT m) where
  ask = lift ask
  local = mapNakadiT . local

-- | For MonadResource.
instance MonadResource m => MonadResource (NakadiT m) where
  liftResourceT = lift . Control.Monad.Trans.Resource.liftResourceT

-- | For MonadState.
instance MonadState s m => MonadState s (NakadiT m) where
  get = lift get
  put = lift . put

instance MonadUnliftIO m => MonadUnliftIO (NakadiT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = NakadiT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runNakadiT r))

-- forall n b. Monad n => NakadiT n b -> n (StT NakadiT b)

-- instance MonadTransControl NakadiT where
--     type StT NakadiT a = a
--     liftWith f =
--     restoreT = NakadiT . const
--     {-# INLINABLE liftWith #-}
--     {-# INLINABLE restoreT #-}

-- instance MonadBaseControl IO m => MonadBaseControl IO (NakadiT m) where
--   type StM (NakadiT b) a = ComposeSt NakadiT b a
--   liftBaseWith = defaultLiftBaseWith
--   restoreM     = defaultRestoreM
--   {-# INLINABLE liftBaseWith #-}
--   {-# INLINABLE restoreM #-}
