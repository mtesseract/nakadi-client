-- | Internal Types

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Network.Nakadi.Internal.Types
  ( module Network.Nakadi.Internal.Types.Config
  , module Network.Nakadi.Internal.Types.Exceptions
  , module Network.Nakadi.Internal.Types.Logger
  , module Network.Nakadi.Internal.Types.Problem
  , module Network.Nakadi.Internal.Types.Service
  , module Network.Nakadi.Internal.Types.Subscription
  , module Network.Nakadi.Internal.Types.Util
  , MonadNakadi
  , MonadNakadiEnv
  ) where

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Exceptions
import           Network.Nakadi.Internal.Types.Logger
import           Network.Nakadi.Internal.Types.Problem
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription
import           Network.Nakadi.Internal.Types.Util

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import qualified Network.Nakadi.Internal.Lenses             as L

-- | Type constraint synonym for encapsulating the monad constraints
-- required by most funtions in this package.
type MonadNakadi m = (MonadIO m, MonadCatch m, MonadThrow m)

-- | Type constraint synonym for encapsulating the monad constraints
-- required by most funtions in this package. Reader Monad version,
-- expects a 'Config' to be available in the current reader
-- environment.
type MonadNakadiEnv r m = (MonadNakadi m, MonadReader r m, L.HasNakadiConfig r Config)
