{-|
Module      : Network.Nakadi.Internal.Logging
Description : Internal logging API
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements internal logging functionality for convenient
logging from within the package.
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.Logging
  ( nakadiLogMsg
  , nakadiLogDebug
  , nakadiLogInfo
  , nakadiLogWarn
  , nakadiLogError
  , module Data.Format
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Monad.Logger
import           Data.Format
import qualified Network.Nakadi.Internal.Lenses  as L

import           Network.Nakadi.Internal.Types

source :: LogSource
source = "nakadi-client"

nakadiLogMsg :: MonadNakadi b m => LogLevel -> Text -> m ()
nakadiLogMsg level msg = do
  config <- nakadiAsk
  nakadiLiftBase $
    case config^.L.logFunc of
      Just logFunc ->
        logFunc source level (toLogStr msg)
      Nothing ->
        pure ()

nakadiLogDebug :: MonadNakadi b m => Text -> m ()
nakadiLogDebug = nakadiLogMsg LevelDebug

nakadiLogInfo :: MonadNakadi b m => Text -> m ()
nakadiLogInfo = nakadiLogMsg LevelInfo

nakadiLogWarn :: MonadNakadi b m => Text -> m ()
nakadiLogWarn = nakadiLogMsg LevelWarn

nakadiLogError :: MonadNakadi b m => Text -> m ()
nakadiLogError = nakadiLogMsg LevelError
