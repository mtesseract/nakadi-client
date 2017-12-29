{-|
Module      : Network.Nakadi.Registry
Description : Implementation of Nakadi Registry API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/registry@ API.
-}

{-# LANGUAGE FlexibleContexts #-}

module Network.Nakadi.Registry
  ( registryPartitionStrategies
  , registryPartitionStrategiesR
  ) where

import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Prelude

path :: ByteString
path = "/registry"

-- | Retrieves supported partitioning strategies from Nakadi. @GET@ to
-- @\/registry\/partition-strategies@.
registryPartitionStrategies ::
  MonadNakadi b m
  => Config' b
  -> m [PartitionStrategy]
registryPartitionStrategies config =
  httpJsonBody config status200 []
  (setRequestMethod "GET" . setRequestPath (path <> "/partition-strategies"))

-- | Retrieves supported partitioning strategies from Nakadi,
-- obtaining configuration from environment.
registryPartitionStrategiesR :: MonadNakadiEnv r m => m [PartitionStrategy]
registryPartitionStrategiesR = do
  config <- nakadiAsk
  registryPartitionStrategies config
