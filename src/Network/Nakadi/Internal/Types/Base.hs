{-|
Module      : Network.Nakadi.Internal.Types.Base
Description : Nakadi Client Base Types (Internal)
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module provides the 'MonadNakadiBase' typeclass and
corresponding instances.
-}

{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Nakadi.Internal.Types.Base where

import           Control.Monad.Base
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Control.Monad.State.Lazy        as State.Lazy
import qualified Control.Monad.State.Strict      as State.Strict
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import qualified Control.Monad.Writer.Lazy       as Writer.Lazy
import qualified Control.Monad.Writer.Strict     as Writer.Strict
import           Network.Nakadi.Internal.Prelude

-- | The 'MonadNakadiBase' typeclass. A typeclass constraint of the form @MonadNakadiBase b m@ is
-- a constraint for the typeclass @MonadNakadi b m@. This typeclass simply encodes the possibility
-- of lifting an action from @b@ to @m@.
class (Monad b, Monad m) => MonadNakadiBase b m where
  -- | Lift an action in the base monad to the monad @m@.
  nakadiLiftBase :: b a -> m a
  default nakadiLiftBase :: (MonadNakadiBase b n, MonadTrans t, m ~ t n) => b a -> m a
  nakadiLiftBase = lift . nakadiLiftBase

instance {-# OVERLAPPING #-} MonadNakadiBase IO IO where
  nakadiLiftBase = identity

instance {-# OVERLAPPING #-} Monad m => MonadNakadiBase (ReaderT r m) (ReaderT r m) where
  nakadiLiftBase = identity

instance {-# OVERLAPPING #-} Monad m => MonadNakadiBase (LoggingT (ReaderT r m)) (LoggingT (ReaderT r m)) where
  nakadiLiftBase = identity

instance {-# OVERLAPPING #-} Monad m => MonadNakadiBase (NakadiBaseT m) (NakadiBaseT m) where
  nakadiLiftBase = identity

instance {-# OVERLAPPABLE #-} MonadNakadiBase b m => MonadNakadiBase b (ReaderT r m)
instance {-# OVERLAPPABLE #-} (MonadNakadiBase b m, Monoid w) => MonadNakadiBase b (Writer.Strict.WriterT w m)
instance {-# OVERLAPPABLE #-} (MonadNakadiBase b m, Monoid w) => MonadNakadiBase b (Writer.Lazy.WriterT w m)
instance {-# OVERLAPPABLE #-} MonadNakadiBase b m => MonadNakadiBase b (LoggingT m)
instance {-# OVERLAPPABLE #-} MonadNakadiBase b m => MonadNakadiBase b (NoLoggingT m)
instance {-# OVERLAPPABLE #-} MonadNakadiBase b m => MonadNakadiBase b (ResourceT m)
instance {-# OVERLAPPABLE #-} (MonadNakadiBase b m) => MonadNakadiBase b (State.Strict.StateT s m)
instance {-# OVERLAPPABLE #-} (MonadNakadiBase b m) => MonadNakadiBase b (State.Lazy.StateT s m)

-- | Applications which need to use some custom, or non-standard, monad as base monad for Nakadi,
-- can use the 'NakadiBaseT' monad transformer.
newtype NakadiBaseT m a = NakadiBaseT
  { runNakadiBaseT :: m a -- ^ The wrapped monadic action.
  } deriving ( Functor, Applicative, Monad, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadReader r, MonadWriter w, MonadState s
             , MonadLogger)

instance MonadTrans NakadiBaseT where
    lift = NakadiBaseT

instance (MonadBase b m) => MonadBase b (NakadiBaseT m) where
  liftBase = liftBaseDefault
