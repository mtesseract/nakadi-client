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

type MonadNakadi m = (MonadIO m, MonadCatch m, MonadThrow m)

type MonadNakadiEnv r m = (MonadNakadi m, MonadReader r m, L.HasNakadiConfig r Config)
