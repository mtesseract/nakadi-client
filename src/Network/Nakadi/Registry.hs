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

module Network.Nakadi.Registry where

import           Control.Lens
import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Prelude

import qualified Network.Nakadi.Internal.Lenses  as L

path :: ByteString
path = "/registry"

registryPartitionStrategies :: MonadNakadi m => Config -> m [PartitionStrategy]
registryPartitionStrategies config =
  httpJsonBody config status200 []
  (setRequestMethod "GET" . setRequestPath (path <> "/partition-strategies"))

registryPartitionStrategiesR :: MonadNakadiEnv r m => m [PartitionStrategy]
registryPartitionStrategiesR = do
  config <- asks (view L.nakadiConfig)
  registryPartitionStrategies config
