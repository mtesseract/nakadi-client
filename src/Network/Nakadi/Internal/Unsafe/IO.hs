{-|
Module      : Network.Nakadi.Unsafe.IO
Description : Support for MonadNakadi IO Instance
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Nakadi-client provides a 'MonadNakadi' instance for the 'IO' monad
accessing a global mutable configuration.

This module implements the functionality for managing this global
configuration.
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.Unsafe.IO where

import           UnliftIO.STM

import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Config

import           Network.Nakadi.Internal.GlobalConfig

-- | Initialize the global configuration used by the 'MonadNakadi' instance
-- for the 'IO' monad via 'newConfigFromEnv'.
initializeGlobalConfigurationFromEnv :: (MonadIO m, MonadThrow m) => m ()
initializeGlobalConfigurationFromEnv = do
  config <- newConfigFromEnv
  atomically $ writeTVar globalConfiguration (Just config)

-- | Sets the global configuration used by the 'MonadNakadi' instance
-- for the 'IO' monad to the provided configuration.
setGlobalConfiguration :: MonadIO m => ConfigIO -> m ()
setGlobalConfiguration = atomically . writeTVar globalConfiguration . Just

-- | Modifies the global configuration used by the 'MonadNakadi' instance
-- for the 'IO' monad using the provided function. If no global configuration has
-- been set so far (using 'initializeGlobalConfigurationFromEnv' or
-- 'setGlobalConfiguration'), this will throw a 'ConfigurationMissing' exception.
modifyGlobalConfiguration :: MonadIO m => (ConfigIO -> ConfigIO) -> m ()
modifyGlobalConfiguration f = atomically $ modifyTVar' globalConfiguration (fmap f)
