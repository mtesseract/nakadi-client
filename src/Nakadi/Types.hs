-- | Library Types

{-# LANGUAGE ConstraintKinds #-}

module Nakadi.Types
  ( module Nakadi.Types.Service
  , module Nakadi.Types.Config
  , MonadNakadi
  , SubscriptionEventStreamContext
  ) where

import           Nakadi.Internal.Types
import           Nakadi.Types.Config
import           Nakadi.Types.Service

import           Control.Exception.Safe
import           Control.Monad.IO.Class

type MonadNakadi m = (MonadIO m, MonadCatch m, MonadThrow m)
