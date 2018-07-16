{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.Unsafe.IO where

import           UnliftIO.STM

import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Config

import           Network.Nakadi.Internal.GlobalConfig

initializeGlobalConfigurationFromEnv :: (MonadIO m, MonadThrow m) => m ()
initializeGlobalConfigurationFromEnv = do
  config <- newConfigFromEnv
  atomically $ writeTVar globalConfiguration (Just config)

setGlobalConfiguration :: MonadIO m => ConfigIO -> m ()
setGlobalConfiguration = atomically . writeTVar globalConfiguration . Just
