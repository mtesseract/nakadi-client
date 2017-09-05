-- | Library Types

{-# LANGUAGE ConstraintKinds #-}

module Network.Nakadi.Types
  ( module Network.Nakadi.Types.Service
  , module Network.Nakadi.Types.Config
  , MonadNakadi
  , SubscriptionEventStreamContext
  , LogFunc
  ) where

import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Types.Config
import           Network.Nakadi.Types.Logger
import           Network.Nakadi.Types.Service

import           Control.Exception.Safe
import           Control.Monad.IO.Class

type MonadNakadi m = (MonadIO m, MonadCatch m, MonadThrow m)
